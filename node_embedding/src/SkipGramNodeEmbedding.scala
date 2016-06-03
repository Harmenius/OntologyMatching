import cc.factorie.{DenseTensor1, Example}
import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingExample, VocabBuilder}
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import com.hp.hpl.jena.rdf.model.{RDFNode, Resource}
import smile.neighbor.KDTree

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

class SkipGramNodeEmbedding() extends NodeEmbeddingModel(){


  val negative = WordSenseOpts.negative.value
  val window = WordSenseOpts.window.value
  val rng = new java.util.Random(5) // https://github.com/iesl/Word2Vec.git
  val sample = WordSenseOpts.sample.value

  def randContext(): Seq[Int] = {
    return Array(1,2,3) // TODO: implement this
  }

  override def process(doc: String): Int = {
    val words = doc.split(' ')
    var concepts = words.map(concept => vocab.getId(concept.toLowerCase))

    if (concepts.length != 3)
      return 0

    val conceptCount = concepts.size - (if (addEdges) 0 else 1)

    for (i <- 0 to 2) {
      if (i != 1) {
        if (concepts(i) != -1) {
          var context = new collection.mutable.ArrayBuffer[Int]
          context += concepts(2 - i) // Add the other concept
          if (bidirectional)
            context += concepts(1)
          else if (addInvertedEdges) {
            if (i == 0) {
              context += concepts(1)
            } else {
              context += getInverted(concepts(1))
            }
          }
          else if (i == 2) // Otherwise parents are not in context so no training required
            break

          if (false) {
            //combineContext)
            trainer.processExample(new SkipGramKHotSamplingExample(this, concepts(i), context, 1))
            (0 until negative).foreach(neg => trainer.processExample(new SkipGramKHotSamplingExample(this, concepts(i), randContext(), -1)))
          } else {
            context = context.filter(v => v != -1)
            context.foreach(context => {
              trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), context, 1))
              (0 until negative).foreach(neg => trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), vocab.getRandWordId, -1)))
            })
          }
        }
      } else {
        // TODO: Add edge learning
      }
    }
    return conceptCount
  }

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

  def getVocab: VocabBuilder = {
    vocab
  }

  var tree : KDTree[Integer] = null

  def buildTree() = {
    val ids = 0 until vocab.size()
    val vectors = ids.map(id => getVector(id)).toArray
    tree = new KDTree[Integer](vectors, ids.map(i => new Integer(i)).toArray)
  }

  def getKNN(vec: Array[Double], k: Integer): Array[(Array[Double], Integer)] = {
    if (tree == null)
      buildTree()
    tree.knn(vec, k).map(n => (n.key, n.value))
  }

  def getInverted(id: Int) : Int = {
    if (id == -1)
      return -1
    val str = vocab.getWord(id)
    vocab.getId("INV" + str)
  }

  def getOntologies : Array[Ontology] = ontology.ontologies

}

case class SkipGramKHotSamplingExample(model: SkipGramNodeEmbedding, word: Int, context: Seq[Int], label: Int) extends Example {
  override def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    val wordEmbedding = model.weights(word).value // access the word's embedding
    val contextEmbedding = new DenseTensor1(model.D, 0)
    context.foreach(c => contextEmbedding += model.weights(c).value) // access the context's embedding
    val score: Double = wordEmbedding.dot(contextEmbedding)
    val exp: Double = math.exp(-score) // TODO : pre-compute exp table
    var objective: Double = 0.0
    var factor: Double = 0.0
    if (label == 1) {
      objective = -math.log1p(exp)
      factor = exp / (1 + exp)
    }
    if (label == -1) {
      objective = -score - math.log1p(exp)
      factor = -1 / (1 + exp)
    }
    if (value ne null) value.accumulate(objective)
    if (gradient ne null) {
      gradient.accumulate(model.weights(word), contextEmbedding, factor)
      context.foreach(c => gradient.accumulate(model.weights(c), wordEmbedding, factor))
    }

  }
}