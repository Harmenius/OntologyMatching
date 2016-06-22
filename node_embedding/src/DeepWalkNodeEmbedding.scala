import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingEmbeddingModel, SkipGramNegSamplingExample}
import com.hp.hpl.jena.rdf.model.{RDFNode, Resource, Statement}

import scala.util.Random
import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, List}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by harmen on 16-6-16.
  */
class DeepWalkNodeEmbedding extends NodeEmbeddingModel {

  val URIs = Array("http://mouse.owl#",
                   "http://human.owl#")
  val LABEL_URI = "http://www.w3.org/2000/01/rdf-schema#label"
  val neighbours = buildNeighbours()

  def buildNeighbours(): Array[Map[String, Array[String]]] = {
    val neighbours = new Array[Map[String, Array[String]]](2)

    for (curOnt <- 0 to 1) {
      val curNeighbours = new mutable.HashMap[String, Array[String]]()
      val onto = ontology.getOntologies(curOnt).asInstanceOf[RDFOntology]
      val edges = onto.getEdges
      for (edge <- edges) {
        val nbs: Array[String] = curNeighbours.getOrElse(edge.split(" ")(0), Array())
        curNeighbours.put(edge.split(" ")(0), nbs :+ edge.split(" ")(2))
        val nbs_ = curNeighbours.getOrElse(edge.split(" ")(2), Array())
        curNeighbours.put(edge.split(" ")(2), nbs_ :+ edge.split(" ")(0))
      }
      neighbours(curOnt) = collection.immutable.HashMap[String, Array[String]](curNeighbours.toList: _*)
    }
    neighbours
  }

  def getNeighbours(curNode: String, curOnt: Int) : Array[String] = {
    neighbours(curOnt).getOrElse(curNode, new Array[String](0))
  }

  def getWeights(neighbors: Seq[String]): Seq[Double] = {
    Seq.fill(neighbors.length)(1.0)
  }

  def getSample(weights: Seq[Double]): Int = {
    var cm = 0.0
    val cms_ = new ArrayBuffer[Double]()
    for ((w, i) <- weights zipWithIndex) {
      cm += w
      cms_ += cm
    }
    val cms = cms_.toArray
    val v = rng.nextDouble()*weights.sum
    for ((w, i) <- cms zipWithIndex) {
      if (w >= v)
        return i
    }
    0
  }

  def other(curOnt: Int): Int = {
    1 - curOnt
  }

  def maybeSwitch(curOnt: Int): Int = {
    // TODO don't hardcode chance
    val p = 0.9
    if (rng.nextDouble() > p)
      return other(curOnt)
    else
      return curOnt
  }

  def check_finished(it: Int): Boolean = {
    return it < nIts
  }

  def end_sentence(): Boolean = {
    val p = 0.97 //TODO: don't hardcode
    rng.nextDouble() > p
  }

  override def workerThread(id: Int, fileLen: Long, printAfterNDoc: Long = 100): Unit = {
    var curOnt = rng.nextInt(2)
    var work = true
    var ndoc = 0

    while(work) {
      ndoc += 1
      val sentence = new StringBuilder()
      val nodes = ontology.getNodes.toList
      var curNode = nodes(rng.nextInt(nodes.length))
      sentence.append(curNode)

      var sentence_end = false
      while(!sentence_end) {
        val neighbors = getNeighbours(curNode, curOnt)
        if (neighbors.isEmpty)
          sentence_end = true // End prematurely
        else {
          val weights = getWeights(neighbors)
          val i = getSample(weights)
          val w = neighbors(i)
          if (vocab.asInstanceOf[MultiVocabBuilder].getVocab(other(curOnt)).getId(w) != -1) {
            // i.e. node is in other onto
            curOnt = maybeSwitch(curOnt)
          }
          sentence.append(" " + w)
          curNode = w
          sentence_end = end_sentence()
        }
      }
      process(sentence.toString())
      if (ndoc % printAfterNDoc == 0 && id == 0) {
        printf("Progress: %d/%d%n", ndoc, nIts)
        store()
      }
      work = check_finished(ndoc)
      curOnt = maybeSwitch(curOnt)
    }
  }

  // Stuff copied from SkipGramNodeEmbedding
  def getVector(node: RDFNode) : Array[Double] = {
    getVector(ontology.toString(node))
  }

  def getVector(index: Int) : Array[Double] = {
    if (vocab == null) {
      this.buildVocab()
    }
    if (weights == null) {
      this.learnEmbeddings()
    }
    val weight = weights(index)
    Array.tabulate(weight.value.size)(i => weight.value(i))
  }

  def getVector(name: String) : Array[Double] = {
    if (vocab == null) {
      this.buildVocab()
    }
    val id = vocab.getId(name)
    if (id == -1)
      return Array(Double.NaN)
    getVector(id)
  }

  // Stuff copied from SKipGramEmbeddingModel
  val negative = opts.negative.value
  val window = opts.window.value
  val rng = new util.Random(5) // set rng seed
  val sample = opts.sample.value.toDouble

  def process(doc: String): Int = {
    // given a document, below line splits by space and converts each word to Int (by vocab.getId) and filters out words not in vocab
    var sen = doc.stripLineEnd.split(' ').map(word => vocab.getId(word)).filter(id => id != -1)
    val wordCount = sen.size

    // subsampling -> speed increase
    if (sample > 0)
      sen = sen.filter(id => subSample(id) != -1)

    val senLength = sen.size
    for (senPosition <- 0 until senLength) {
      val currWord = sen(senPosition)
      val b = rng.nextInt(window)
      // make the contexts
      val contexts = new collection.mutable.ArrayBuffer[Int]
      for (a <- b until window * 2 + 1 - b) if (a != window) {
        val c = senPosition - window + a
        if (c >= 0 && c < senLength)
          contexts += sen(c)
      }
      // make the examples
      contexts.foreach(context => {
        trainer.processExample(new SkipGramNegSamplingExample(this, currWord, context, 1))
        (0 until negative).foreach(neg => trainer.processExample(new SkipGramNegSamplingExample(this, currWord, vocab.getRandWordId, -1)))
      })
    }
    return wordCount
  }

  def subSample(word: Int): Int = {
    val prob = vocab.getSubSampleProb(word) // pre-computed to avoid sqrt call every time.
    val alpha = rng.nextInt(0xFFFF) / 0xFFFF.toDouble
    if (prob < alpha) { return -1 }
    else return word
  }
}

