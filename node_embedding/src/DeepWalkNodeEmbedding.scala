import cc.factorie.{DenseTensor1, Example}
import cc.factorie.app.nlp.embeddings.{TensorUtils}
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import com.hp.hpl.jena.rdf.model.{RDFNode}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by harmen on 16-6-16.
  */
class DeepWalkNodeEmbedding extends SkipGramNodeEmbedding {
  var synonyms: mutable.HashMap[String, String] = null

  def setOntology(ontology: SharedOntology) = {
    this.ontology = ontology
  }


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
          val w_ = if (synonyms == null) w else synonyms(w)
          if (vocab.asInstanceOf[MultiVocabBuilder].getVocab(other(curOnt)).getId(w_) != -1) {
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
        //store()
      }
      work = check_finished(ndoc)
      curOnt = maybeSwitch(curOnt)
    }
  }

  // Stuff copied from SkipGramNodeEmbedding
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
    val weight = weights(index)
    Array.tabulate(weight.value.size)(i => weight.value(i))
  }

  override def getVector(name: String) : Array[Double] = {
    if (vocab == null) {
      this.buildVocab()
    }
    val id = vocab.getId(name)
    if (id == -1)
      return Array(Double.NaN)
    getVector(id)
  }

  // Stuff copied from SKipGramEmbeddingModel
  // Probably can be removed now that it extends it
  override val negative = opts.negative.value
  override val window = opts.window.value
  override val rng = new java.util.Random(5) // set rng seed
  override val sample = opts.sample.value.toDouble

  override def process(doc: String): Int = {
    // given a document, below line splits by space and converts each word to Int (by vocab.getId) and filters out words not in vocab
    // id of a word is its freq-rank in the corpus
    var sen = doc.stripLineEnd.split(' ').map(word => vocab.getId(word.toLowerCase())).filter(id => id != -1)
    val wordCount = sen.size

    //
    // subsampling the words : refer to Google's word2vec NIPS paper to understand this
    //
    if (sample > 0)
      sen = sen.filter(id => subSample(id) != -1)

    val senLength = sen.size
    for (senPosition <- 0 until senLength) {
      val currWord = sen(senPosition)

      //
      // dynamic window-size as in word2vec.
      //
      val b =  rng.nextInt(window)

      //
      // make the contexts
      //
      val contexts = new mutable.ArrayBuffer[Int]
      for (a <- b until window * 2 + 1 - b) if (a != window) {
        val c = senPosition - window + a
        if (c >= 0 && c < senLength)
          contexts += sen(c)
      }

      // predict the sense of the word given the contexts. P(word-sense | word, contexts)
      var rightSense = 0
      rightSense  = cbow_predict_kmeans(currWord, contexts)

      // make the examples. trainer is HogWild!
      contexts.foreach(context => {
        // postive example
        trainer.processExample(new MSCBOWSkipGramNegSamplingExample(this, currWord, rightSense, context, 1))
        // for each POS example, make negative NEG examples.
        //vocab.getRandWordId would get random word proportional (unigram-prob)^(0.75).
        (0 until negative).foreach(neg => trainer.processExample(new MSCBOWSkipGramNegSamplingExample(this, currWord, rightSense, vocab.getRandWordId, -1)))

      })
    }
    return wordCount
  }

  val learnMultiVec = Array.fill[Boolean](V)(true)
  val clusterCount  = Array.ofDim[Int](V, 3)
  val clusterCenter = Array.ofDim[DenseTensor1](V, 3)
  val sense_weights = (0 until V).map(v => (0 until 3).map(s => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))))

  def cbow_predict_kmeans(word: Int, contexts: Seq[Int]): Int = {
    val contextsEmbedding = new DenseTensor1(D, 0)
    contexts.foreach(context => contextsEmbedding.+=(weights(context).value))
    var sense = 0

    var minDist = Double.MaxValue
    for (s <- 0 until 3) {
      val mu = clusterCenter(word)(s)/(clusterCount(word)(s))
      val dist = 1 - TensorUtils.cosineDistance(contextsEmbedding, mu)
      if (dist < minDist) {
        minDist = dist
        sense = s
      }
    }
    // update the cluster center
    clusterCenter(word)(sense).+=(contextsEmbedding)
    clusterCount(word)(sense) += 1
    sense
  }

  def subSample(word: Int): Int = {
    val prob = vocab.getSubSampleProb(word) // pre-computed to avoid sqrt call every time.
    val alpha = rng.nextInt(0xFFFF) / 0xFFFF.toDouble
    if (prob < alpha) { return -1 }
    else return word
  }

  def setSynonyms(synonyms: mutable.HashMap[String, String]): Unit = {
    this.synonyms = synonyms
  }
}

class MSCBOWSkipGramNegSamplingExample(model: DeepWalkNodeEmbedding, word: Int, sense : Int, context : Int, label: Int) extends Example {

  // to understand the gradient and objective refer to : http://arxiv.org/pdf/1310.4546.pdf
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {

    val wordEmbedding = model.sense_weights(word)(sense).value
    val contextEmbedding = model.weights(context).value


    val score: Double = wordEmbedding.dot(contextEmbedding)
    val exp: Double = math.exp(-score) // TODO : pre-compute expTable similar to word2vec

    var objective: Double = 0.0
    var factor: Double = 0.0

    // for POS Label
    if (label == 1) {
      objective = -math.log1p(exp) // log1p -> log(1+x)
      factor = exp / (1 + exp)
    }
    // for NEG Label
    if (label == -1) {
      objective = -score - math.log1p(exp)
      factor = -1 / (1 + exp)
    }

    if (value ne null) value.accumulate(objective)
    if (gradient ne null) {
      gradient.accumulate(model.sense_weights(word)(sense), contextEmbedding, factor)
      // don;t update if global_weights is fixed.
      gradient.accumulate(model.weights(context), wordEmbedding, factor)
    }

  }
}
