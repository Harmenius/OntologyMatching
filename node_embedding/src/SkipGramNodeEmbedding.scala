import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingExample, VocabBuilder}
import com.hp.hpl.jena.rdf.model.{RDFNode, Resource}
import smile.neighbor.KDTree

import scala.util.control.Breaks._

class SkipGramNodeEmbedding() extends NodeEmbeddingModel(){


  val negative = WordSenseOpts.negative.value
  val window = WordSenseOpts.window.value
  val rng = new java.util.Random(5) // https://github.com/iesl/Word2Vec.git
  val sample = WordSenseOpts.sample.value

  override def process(doc: String): Int = {
    val words = doc.split(' ')
    var concepts = words.map(concept => vocab.getId(concept.toLowerCase))

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

          if (false) //combineContext)
            println("Combined context not implemented yet") //TODO: implement context combining
          else
            context = context.filter(v => v != -1)
            context.foreach(context => {
              trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), context, 1))
              (0 until negative).foreach(neg => trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), vocab.getRandWordId, -1)))
            })
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
    return tree.knn(vec, k).map(n => (n.key, n.value))
  }

  def getInverted(id: Int) : Int = {
    if (id == -1)
      return -1
    val str = vocab.getWord(id)
    vocab.getId("INV" + str)
  }

  def getOntologies : Array[Ontology] = ontology.ontologies

}
