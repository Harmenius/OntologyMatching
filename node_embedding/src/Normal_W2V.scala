import java.io.{BufferedInputStream, BufferedReader, FileInputStream, InputStreamReader}
import java.util.zip.GZIPInputStream

import Evaluator.KDTree
import cc.factorie.DenseTensor1
import cc.factorie.app.nlp.embeddings.{TensorUtils, VocabBuilder}
import cc.factorie.model.Weights
import com.hp.hpl.jena.rdf.model.RDFNode

import math.sqrt
import scala.collection.mutable
import scala.util.Try

/**
  * Created by harmen on 20-6-16.
  */
object Normal_W2V {

  WordSenseOpts.inputFilename.setValue("Data/pretrained/glove.6B.50d.txt")
  WordSenseOpts.binary.setValue(true)
  val model = new NormalEmbeddingModel()

  def main(args: Array[String]): Unit = {
    model.buildVocab()
    model.learnEmbeddings()
    Evaluator.model = model
    val alignment = align()

    //Visualizer.hist_alignment(alignment)
    //alignment.set_threshold(0.075)
    val truth = Evaluator.loadTruth()
    //Visualizer.compare_hist(alignment, truth, calc)
    val dice = Evaluator.compare(alignment, truth)
    //val synonyms = Evaluator.loadTruth(WordSenseOpts.synonyms.value)
    //val dice_ = Evaluator.compare(synonyms, truth)
    printf("The result of what you have been working for for months: %s%n", dice)
    //printf("For reference, just the synonyms scores: %s%n", dice_)
    //show_alignment(alignment, truth)
    Evaluator.plot_roc(alignment, truth, this.calc)
  }

  def align(): Alignment = {
    val o1 = model.getOntologies(0)
    val o2 = model.getOntologies(1)
    println("Building trees for aligning")
    val synonyms = mutable.HashMap[String, String]() //model.loadSynonyms()

    val nodes1 = o1.getNodes.map(model.getRootSynonym(_, synonyms)).toSet[String].toArray[String] // Don't grab all synonyms too
    val vectors1 : Array[Array[Double]] = nodes1.map(n => model.getVector(n))
    val nv1 = vectors1.zip(nodes1).filterNot{case (v,_) => v(0).isNaN}.unzip

    val nodes2 = o2.getNodes.map(model.getRootSynonym(_, synonyms)).toSet[String].toArray[String]
    val vectors2 : Array[Array[Double]] = nodes2.map(n => model.getVector(n))
    val nv2 = vectors2.zip(nodes2).filterNot{case (v,_) => v(0).isNaN}.unzip

    val tree1 = KDTree(nv1)
    val tree2 = KDTree(nv2)
    println("Trees built")

    println("Starting with aligning")
    val alignment = new Alignment
    for(n1 <- nv1._2) {
      for(n2 <- tree2.knn(model.getVector(n1), 50)) {
        val n2_ = n2.value
        if (!n1.contains("blank") && !n2_.contains("blank"))
          alignment.add(n1, n2.value, calc(n1, n2.value))
      }
    }
    println("Done aligning")
    alignment
  }

  def calc(node1: String, node2: String): Double = {
    if (node1 == node2)
      return -0.1
    val v1 = model.getVector(node1)
    val v2 = model.getVector(node2)
    val v = sqrt((v1 zip v2 map {case (x,y)=> (x - y) * (x - y)}).sum) // Euclidean vector distance
    if (v.isNaN)
      100000d
    else
      v
  }
}

class SeparatedVocab() extends VocabBuilder(5e5.toInt, 3e6.toInt, 0.7) {

  override def addWordToVocab(keys: String): Unit = {
    for (key <- keys.split("[ _]")) {
      super.addWordToVocab(key)
    }
  }
}

class NormalEmbeddingModel() extends SkipGramNodeEmbedding {

  this.vocab = new SeparatedVocab()

  override def loadEmbeddings(mapping: scala.collection.mutable.HashMap[String, String] = null) {
    val in = new BufferedReader(new InputStreamReader(
      storeInBinary match {
        case true => new FileInputStream(WordSenseOpts.inputFilename.value)
        case false => new GZIPInputStream(new BufferedInputStream(new FileInputStream(WordSenseOpts.inputFilename.value)))
      }, encoding))
    // format :
    // <vocabsize><space><dim-size><newline>
    // <word>[<space><embedding(word)(d)>]*dim-size<newline>
    in.readLine() // Skip header
    var line : String = ""
    val weights = Array.fill[Weights](vocab.size)(Weights(TensorUtils.setToRandom1(new DenseTensor1(D))))
    while({line = in.readLine; line != null}) {
      val word = line.split(" ").apply(0)

      if (vocab.getId(word) != -1) {
        val weight: Array[String] = line.split(" ").drop(1)
        val weightTensor = new DenseTensor1(weight.length)
        weight.zipWithIndex foreach (t => weightTensor.update(t._2, t._1.toDouble))
        weights(vocab.getId(word)) = Weights(weightTensor)
      }
    }
    this.weights = weights
    in.close()
  }

  override protected def buildVocabRDF(corpus: String, synonyms: scala.collection.mutable.HashMap[String, String], vocab_i: Int) = {
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
        //vocab.asInstanceOf[SynonymVocabBuilder].addSynonymToVocab(n2, n1)
      } else {
        vocab.asInstanceOf[SeparatedVocab].addWordToVocab(n1)
        vocab.asInstanceOf[SeparatedVocab].addWordToVocab(n2)
      }
    }
  }

  override def buildVocab(all: Boolean): Unit = {
    for ((one_corpus, i) <- corpusses.split(";").zipWithIndex) {
      buildVocab(one_corpus, new scala.collection.mutable.HashMap[String, String](), i)
    }
    // save the vocab if the user provides the filename save-vocab
    if (saveVocabFilename.nonEmpty) {
      println("Saving Vocab into " + saveVocabFilename)
      vocab.saveVocab(saveVocabFilename, if (storeInBinary) 1 else 0, encoding) // for every word, <word><space><count><newline>
      println("Done Saving Vocab")
    }

    vocab.sortVocab(minCount, ignoreStopWords, maxVocabSize) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    Try(vocab.buildSubSamplingTable(opts.sample.value)) // precompute subsampling table
    V = vocab.size()
    train_nodes = vocab.trainWords()
    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_nodes)
  }

  override def getVector(node: RDFNode) : Array[Double] = {
    getVector(ontology.toString(node))
  }

  override def getVector(index: Int) : Array[Double] = {
    if (vocab == null) {
      this.buildVocab()
    }
    if (weights == null) {
      this.learnEmbeddings()
    }
    getVector(vocab.getWord(index))
  }

  override def getVector(name: String) : Array[Double] = {
    if (vocab == null) {
      this.buildVocab()
    }
    var count = 0
    var all_weight = Array.fill[Double](D)(0)
    for (w <- name.split("[ _]")) {
      val id = vocab.getId(w)
      if (id != -1) {
        count += 1
        val weight = weights(id)
        val weight_ = Array.tabulate(weight.value.size)(i => weight.value(i))
        all_weight = all_weight zip weight_ map (t => t._1 + t._2)
      }
    }
    all_weight = all_weight.map(_/count)
    all_weight
  }

  // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
  override def process(doc: String): Int = {
    1
  }
}
