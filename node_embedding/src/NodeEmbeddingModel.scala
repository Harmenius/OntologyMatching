import cc.factorie.app.nlp.embeddings.{LiteHogwildTrainer => HogWildTrainer}
import cc.factorie.model.{Parameters, Weights}
import cc.factorie.optimize.{AdaGradRDA, Trainer}
import cc.factorie.la.{DenseTensor1, Tensor}
import cc.factorie.util.Threading
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.collection.JavaConverters._
import cc.factorie.app.nlp.embeddings.{EmbeddingOpts, FastLineReader, TensorUtils, VocabBuilder, WordEmbeddingModel}
import com.hp.hpl.jena.rdf.model.RDFNode

import scala.collection.mutable
import scala.util.Try

abstract class NodeEmbeddingModel() extends WordEmbeddingModel(WordSenseOpts) {
  //implicit def bool2int(b:Boolean): Int = if (b) 1 else 0 // Converts booleans to ints when needed
  //implicit def int2bool(i:Int): Boolean = if(i==0) false else true // Convert ints to booleans when needed
  // Algo related
  protected val addEdges = WordSenseOpts.includeEdgeLabels.value
  protected val bidirectional = WordSenseOpts.bidirectional.value // Add both concepts with context (other concept, edge)
  protected val addInvertedEdges = !bidirectional && WordSenseOpts.invertedEdges.value // If not bidirectional, add concept with context (parent, "inv"+edge)
  protected val combineContext = WordSenseOpts.combineContext.value // Process edge and node value combined, not separately

  // IO Related
  private val storeInBinary = opts.binary.value // binary=1 will make both vocab file (optional) and embeddings in .gz file
  private val loadVocabFilename = opts.loadVocabFile.value // load the vocab file. Very useful for large corpus should you run multiple times
  private val saveVocabFilename = opts.saveVocabFile.value // save the vocab into a file. Next time for the same corpus, load it . Saves lot of time on large corpus
  private val encoding = opts.encoding.value // Default is ISO-8859-15. Note: Blake server follows iso-5589-1 (david's GoogleEmbeddingcode has this. shouldn;t be ISO-8859-15) or  iso-5589-15 encoding (I see this) ??
  private val includeEdgeLabels = WordSenseOpts.includeEdgeLabels.value
  protected val corpusses = WordSenseOpts.corpusses.value
  protected var current_corpus = ""

  protected val ontology : SharedOntology = new SharedOntology(corpusses)
  private var train_nodes: Long = 0

  def getRootSynonym(w: String, synonyms: mutable.HashMap[String, String]): String = {
    var w_ = Try(synonyms(w)).getOrElse(w)
    while(synonyms.contains(w_) && w_ != w)
      w_ = synonyms(w_)
    w_
  }

  private def buildVocabRDF(corpus: String, synonyms: mutable.HashMap[String, String]) = {
    println("... from RDF")
    // From each edge, grab subject and object
    val edgeNames: List[String] = ontology.getOntology(corpus).getEdges.map(_.toLowerCase).toList
    val names: List[List[String]] = edgeNames.map(y => List(y.split(" ").head, y.split(" ").last))
    val edgeLabels: List[String] = edgeNames.map(y => y.split(" ")(1))
    for ((lbl, name2) <- edgeLabels zip names) {
      var n1 :: n2 :: _ = name2
      val n1_ = getRootSynonym(n1, synonyms)
      val n2_ = getRootSynonym(n2, synonyms)

      if (lbl endsWith "hasrelatedsynonym") {
        vocab.asInstanceOf[SynonymVocabBuilder].addSynonymToVocab(n2, n1)
      } else {
        vocab.asInstanceOf[SynonymVocabBuilder].addSynonymToVocab(n1, n1_)
        vocab.asInstanceOf[SynonymVocabBuilder].addSynonymToVocab(n2, n2_)
      }
    }
  }

  private def buildVocabCSV(corpus: String, Synonyms: mutable.HashMap[String, String]) = {
    println("... from CSV")
    val corpusLineItr = corpus.endsWith(".gz") match {
      case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(corpus)), encoding).getLines
      case false => io.Source.fromInputStream(new FileInputStream(corpus), encoding).getLines
    }
    while (corpusLineItr.hasNext) {
      val line = corpusLineItr.next
      if (includeEdgeLabels) {
        var words = line.stripLineEnd.split(' ')
        words.foreach(w => vocab.addWordToVocab(w.toLowerCase))
        if (addInvertedEdges)
          vocab.addWordToVocab("INV" + words(1))
      } else {
        var words = line.stripLineEnd.split(' ')
        Array(0, 2).foreach(i => vocab.addWordToVocab(words(i).toLowerCase)) // Only first and third labels are nodes/concepts
      }
    }
  }

  override def buildVocab(): Unit = buildVocab(true)

  def loadSynonyms(t: Float = 0.6f): mutable.HashMap[String, String] = {
    val alignment = Evaluator.loadTruth(WordSenseOpts.synonyms.value)
    val alignment_ = new mutable.HashMap[String, String]()
    for ((o, s, v: Double) <- alignment.alignments.asScala) {
      if (v >= t) {
        if (o != s)
          alignment_.put(o, s)
      }
    }
    alignment_
  }

  // total # of nodes in the corpus. Needed to calculate the distribution of the work among threads and seek points of corpus file
  // Component-1
  def buildVocab(all: Boolean): Unit = {
    vocab = new SynonymVocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor
    val synonyms = loadSynonyms()
    if (loadVocabFilename.isEmpty) {
      for (one_corpus <- corpusses.split(";")) {
        buildVocab(one_corpus, synonyms)
      }
      // save the vocab if the user provides the filename save-vocab
      if (saveVocabFilename.nonEmpty) {
        println("Saving Vocab into " + saveVocabFilename)
        vocab.saveVocab(saveVocabFilename, if (storeInBinary) 1 else 0, encoding) // for every word, <word><space><count><newline>
        println("Done Saving Vocab")
      }
    } else {
      println("Loading cached vocabulary")
      vocab.loadVocab(loadVocabFilename, encoding)
    }
    vocab.sortVocab(minCount, ignoreStopWords, maxVocabSize) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    vocab.buildSubSamplingTable(opts.sample.value) // precompute subsampling table
    V = vocab.size()
    train_nodes = vocab.trainWords()
    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_nodes)
  }


  def buildVocab(corpus: String, synonyms: mutable.HashMap[String, String]): Unit = {
      println("Building vocabulary")
      if (corpus.contains(".owl") || corpus.contains(".rdf")) {
        buildVocabRDF(corpus, synonyms)
      } else {
        buildVocabCSV(corpus, synonyms)
      }
  }

  def loadEmbeddings(): Unit = {
    val in = new BufferedReader(new InputStreamReader(
      storeInBinary match {
        case true => new FileInputStream(WordSenseOpts.embeddingOutFile.value)
        case false => new GZIPInputStream(new BufferedInputStream(new FileInputStream(WordSenseOpts.embeddingOutFile.value)))
      }, encoding))
    // format :
    // <vocabsize><space><dim-size><newline>
    // <word>[<space><embedding(word)(d)>]*dim-size<newline>
    in.readLine() // Skip header
    var line : String = ""
    val weights = new Array[Weights](vocab.size)
    while({line = in.readLine; line != null}) {
      val word = line.split(" ").apply(0)
      val weight : Array[String] = line.split(" ").drop(1)
      val weightTensor = new DenseTensor1(weight.length)
      weight.zipWithIndex foreach (t => weightTensor.update(t._2, t._1.toDouble))
      weights(vocab.getId(word)) = Weights(weightTensor)
    }
    this.weights = weights
    in.close()
  }

  // Component-2
  override def learnEmbeddings(): Unit = {
    if (!WordSenseOpts.inputFilename.value.isEmpty)
    {
      println("Loading Embeddings")
      loadEmbeddings()
      println("Done loading embeddings")
    } else {

      println("Learning Embeddings")
      optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
      weights = (0 until V).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
      optimizer.initializeWeights(this.parameters)
      trainer = new HogWildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)

      val threadIds = (0 until threads).map(i => i)
      //val fileLen = new File(current_corpus).length
      val fileLen = ontology.getEdges.size
      workerThread(0, fileLen, fileLen/100)
      println("Done learning embeddings. ")
      if (!WordSenseOpts.embeddingOutFile.value.isEmpty)
        store()
    }
  }

  // Component-3
  override def store(): Unit = {
    println("Now, storing the embeddings .... ")

    // Only store embeddings that are in the ontologies to be matched
    val o1 = ontology.ontologies(0)
    val o2 = ontology.ontologies(1)
    val edges = Set() ++ o1.getNodes ++ o2.getNodes

    val out = storeInBinary match {
      case true => new java.io.PrintWriter(outputFilename, encoding)
      case false => new OutputStreamWriter(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(outputFilename))), encoding)
    }
    // format :
    // <vocabsize><space><dim-size><newline>
    // <word>[<space><embedding(word)(d)>]*dim-size<newline>
    out.write("%d %d\n".format(V, D))
    for (v <- 0 until V) {
      val w = vocab.getWord(v)
      if(edges.contains(w)) {
        out.write(w + " ")
        val embedding = weights(v).value
        for (d <- 0 until D)
          out.write(embedding(d) + " ")
        out.write("\n")
        out.flush()
      }
    }
    out.close()
    println("Done storing embeddings")
  }


  override protected def workerThread(id: Int, fileLen: Long, printAfterNDoc: Long = 10000): Unit = {
    val skipBytes: Long = fileLen / threads * id // fileLen now pre-computed before passing to all threads. skip bytes. skipped bytes is done by other workers
    val edgeItr = ontology.getEdges
    var word_count: Long = 0
    var work = true
    var ndoc = 0
    val total_words_per_thread = train_nodes / threads // worker amount .
    while (edgeItr.hasNext && work) {
      word_count += process(edgeItr.next) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?.
      ndoc += 1
      if (id == 0 && ndoc % printAfterNDoc == 0) {  // print the process after processing 100 docs in 1st thread. It approx reflects the total progress
        println("Progress : " + ndoc / fileLen.toDouble * 100 + " %")
      }
      //work = word_count <= total_words_per_thread // Once, word_count reaches this limit, ask worker to end
    }
    // println("thread :" + id + " word count : " + word_count)
  }

  // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
  protected def process(doc: String): Int
}
