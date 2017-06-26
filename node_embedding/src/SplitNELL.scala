import scala.collection.mutable

/**
  * Created by harmen on 6-7-16.
  */
object SplitNELL {

  def main(args: Array[String]) {
    val truthfile = "/home/harmen/scriptiecode/node_embedding/Data/NELL_clean.csv"
    val rng = new java.util.Random()

    val nell = new CSVOntology(truthfile)
    val o1 = new mutableOntology()
    val o2 = new mutableOntology()
    val truth = new mutableOntology()

    for (node <- nell.getNodes) {
      if (node.hashCode % 2 == 0)
        o1.addNode(node)
      else
        o2.addNode(node)
    }
    for (edge <- nell.getEdges) {
      val n1 = edge.split(" ").head
      val n2 = edge.split(" ").last
      if (o1.getNodeset.contains(n1) && o1.getNodeset.contains(n2)) {
        o1.addEdge(n1, n2)
      } else if (o2.getNodeset.contains(n1) && o2.getNodeset.contains(n2)) {
        o2.addEdge(n1, n2)
      } else if (o2.getNodeset.contains(n1)) {
        truth.addEdge(n1, n2)
      } else {
        truth.addEdge(n2, n1)
      }
    }


    var portion = 0.2
    def getPortion(edges: Iterator[String]): Iterator[String] = {
      val p = portion * 100
      edges.filter(s => s.hashCode % 100 < p)
    }

    val synonyms = getPortion(truth.getEdges)
    val synonymmap = new mutable.HashMap[String, String]
    for (s <- synonyms) {
      val n1 = s.split(" ").head
      val n2 = s.split(" ").last
      synonymmap.put(n1, n2)
      synonymmap.put(n2, n1)
    }

    val model = new DeepWalkNodeEmbedding()
    model.setOntology(new SharedOntology(Array(o1, o2)))
    model.setSynonyms(synonymmap)
  }
}
