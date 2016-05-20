package cc.factorie.app.nlp.embeddings
import cc.factorie.app.nlp.embeddings.{LiteHogwildTrainer => HogWildTrainer}
import cc.factorie.model.{Parameters, Weights}
import cc.factorie.optimize.{Trainer, AdaGradRDA}
import cc.factorie.la.DenseTensor1
import cc.factorie.util.Threading
import java.io.{File, PrintWriter, OutputStreamWriter, FileOutputStream, FileInputStream, BufferedOutputStream}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}


abstract class NodeEmbeddingModel(override val opts: EmbeddingOpts) extends WordEmbeddingModel(opts) {
  implicit def bool2int(b:Boolean): Int = if (b) 1 else 0 // Converts booleans to ints when needed
  // Algo related
  protected val includeEdges = opts.includeEdgeLabels.value // Add edges to context
  protected val bidirectional = opts.bidirectional.value // Add both concepts with context (other concept, edge)
  protected val addInvertedEdges = !bidirectional && opts.invertedEdges.value // If not bidirectional, add concept with context (parent, "inv"+edge)
  protected val combineContext = opts.combineContext.value // Process edge and node value combined, not separately

  // IO Related
  private val storeInBinary = opts.binary.value // binary=1 will make both vocab file (optional) and embeddings in .gz file
  private val loadVocabFilename = opts.loadVocabFile.value // load the vocab file. Very useful for large corpus should you run multiple times
  private val saveVocabFilename = opts.saveVocabFile.value // save the vocab into a file. Next time for the same corpus, load it . Saves lot of time on large corpus
  private val encoding = opts.encoding.value // Default is ISO-8859-15. Note: Blake server follows iso-5589-1 (david's GoogleEmbeddingcode has this. shouldn;t be ISO-8859-15) or  iso-5589-15 encoding (I see this) ??
  private val includeEdgeLabels = opts.includeEdgeLabels.value

  private var train_nodes: Long = 0 // total # of nodes in the corpus. Needed to calculate the distribution of the work among threads and seek points of corpus file
  // Component-1
  override def buildVocab(): Unit = {
    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor
    println("Building Vocab")
    if (loadVocabFilename.isEmpty) {
      val corpusLineItr = corpus.endsWith(".gz") match {
        case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(corpus)), encoding).getLines
        case false => io.Source.fromInputStream(new FileInputStream(corpus), encoding).getLines
      }
      while (corpusLineItr.hasNext) {
        val line = corpusLineItr.next
        if (opts.includeEdgeLabels.value){ //TODO: implement edges
          println("Edge labels no implemented yet")
          System.exit(1)
        }
        var words = line.stripLineEnd.split(' ')
        Array(0,2).foreach(i => vocab.addWordToVocab(words(i).toLowerCase)) // Only first and third labels are nodes/conceps
      }
    }
    else vocab.loadVocab(loadVocabFilename, encoding)

    vocab.sortVocab(minCount, ignoreStopWords, maxVocabSize) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    vocab.buildSubSamplingTable(opts.sample.value) // precompute subsampling table
    V = vocab.size()
    train_nodes = vocab.trainWords()
    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_nodes)
    // save the vocab if the user provides the filename save-vocab
    if (saveVocabFilename.nonEmpty) {
      println("Saving Vocab into " + saveVocabFilename)
      vocab.saveVocab(saveVocabFilename, storeInBinary, encoding) // for every word, <word><space><count><newline>
      println("Done Saving Vocab")
    }

  }

  // Component-2
  override def learnEmbeddings(): Unit = {
    println("Learning Embeddings")
    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    weights = (0 until V).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)
    trainer = new HogWildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
    val threadIds = (0 until threads).map(i => i)
    val fileLen = new File(corpus).length
    Threading.parForeach(threadIds, threads)(threadId => workerThread(threadId, fileLen))
    println("Done learning embeddings. ")
    //store()
  }

  // Component-3
  override def store(): Unit = {
    println("Now, storing the embeddings .... ")
    val out = storeInBinary match {
      case 0 => new java.io.PrintWriter(outputFilename, encoding)
      case 1 => new OutputStreamWriter(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(outputFilename))), encoding)
    }
    // format :
    // <vocabsize><space><dim-size><newline>
    // <word>[<space><embedding(word)(d)>]*dim-size<newline>
    out.write("%d %d\n".format(V, D))
    for (v <- 0 until V) {
      out.write(vocab.getWord(v) + "\n")
      val embedding = weights(v).value
      for (d <- 0 until D)
        out.write(embedding(d) + " ")
      out.write("\n")
      out.flush()
    }
    out.close()
    println("Done storing embeddings")
  }


  override protected def workerThread(id: Int, fileLen: Long, printAfterNDoc: Long = 100): Unit = {
    val skipBytes: Long = fileLen / threads * id // fileLen now pre-computed before passing to all threads. skip bytes. skipped bytes is done by other workers
    val lineItr = new FastLineReader(corpus, skipBytes, encoding)
    var word_count: Long = 0
    var work = true
    var ndoc = 0
    val total_words_per_thread = train_nodes / threads // worker amount .
    while (lineItr.hasNext && work) {
      word_count += process(lineItr.next) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?.
      ndoc += 1
      if (id == 1 && ndoc % printAfterNDoc == 0) {  // print the process after processing 100 docs in 1st thread. It approx reflects the total progress
        println("Progress : " + word_count / total_words_per_thread.toDouble * 100 + " %")
      }
      work = word_count <= total_words_per_thread // Once, word_count reaches this limit, ask worker to end
    }
    // println("thread :" + id + " word count : " + word_count)
  }

  // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
  protected def process(doc: String): Int
}