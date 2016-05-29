import com.hp.hpl.jena.rdf.model.{RDFNode, Resource, Statement}
import smile.neighbor.KDTree
import smile.plot.PlotCanvas

import scala.collection.JavaConverters._
import math.sqrt
import scala.collection.mutable
import scala.util.Try

object Evaluator {

  def compare(alignment: Alignment, truth: Alignment) : Double = {
    val alignmentset = alignment.alignments.asScala.toSet[(String, String, Double)].map{case (s: String, o:String, v: Double) => (s, o)}
    val truthset = truth.alignments.asScala.toSet[(String, String, Double)].map{case (s: String, o: String, v: Any) => (s, o)}
    val N = alignmentset.size
    val M = truthset.size
    var count : Int = 0
    for (pair : (String, String) <- alignmentset) {
      if (truthset.contains(pair) || truthset.contains(pair.swap))
        count+=1
    }
    2f * count / (N + M)
  }

  def loadTruth() : Alignment = {
    val truthfile = WordSenseOpts.truthfile.value
    val truthontology = new RDFOntology(truthfile, false)
    val truthalignment = new Alignment
    val edgemap = mutable.HashMap[String, String]()
    for (edge <- truthontology.getEdges) {
      //val (obj : String) :: (subj : String) :: _ = edge.split(" ")
      val subj = edge.split(" ")(0)
      val obj  = edge.split(" ")(2)
      if(Try(subj.toDouble).isFailure && subj != "=") { // Not relation value or type
        if(edgemap.contains(obj)) {
          truthalignment.add(subj, edgemap.get(obj).get, 1)
        } else {
          edgemap.put(obj, subj)
        }
      }
    }
    truthalignment
  }

  def main(args: Array[String]) {
    val alignment = makeAlignment()
    //val values : Array[Double] = alignment.alignments.toArray.map{case (x,y,v : Double) => v}
    //val plotimage : PlotCanvas = plot(values)
    //showplot(plotimage)
    val truth = loadTruth()
    val dice = compare(alignment, truth)
    printf("The result of what you have been working for for months: %s%n", dice)
  }

  class PlotGUI extends swing.MainFrame {
    title = "Plot"
    preferredSize = new swing.Dimension(500, 500)
  }

  val opts  = WordSenseOpts
  val model = new SkipGramNodeEmbedding()

  def calc(node1: String, node2: String): Double = {
    val v1 = model.getVector(node1)
    val v2 = model.getVector(node2)
    val v = sqrt((v1 zip v2 map {case (x,y)=> (x - y) * (x - y)}).sum) // Euclidean vector distance
    if (v.isNaN)
      100000d
    else
      v
  }

  object KDTree {
    def apply(pair: (Array[Array[Double]], Array[String])) : KDTree[String] = new KDTree(pair._1, pair._2)
  }

  def makeAlignment() : Alignment = {
    model.buildVocab(false)
    model.learnEmbeddings()
    val o1 = model.getOntologies(0)
    val o2 = model.getOntologies(1)

    println("Building trees for aligning")
    val nodes1 = o1.getNodes.toArray
    val vectors1 : Array[Array[Double]] = nodes1.map(n => model.getVector(n))
    val nv1 = vectors1.zip(nodes1).filterNot{case (v,_) => v(0).isNaN}.unzip

    val nodes2 = o2.getNodes.toArray
    val vectors2 : Array[Array[Double]] = nodes2.map(n => model.getVector(n))
    val nv2 = vectors2.zip(nodes2).filterNot{case (v,_) => v(0).isNaN}.unzip

    val tree1 = KDTree(nv1)
    val tree2 = KDTree(nv2)
    println("Trees built")

    println("Starting with aligning")
    val alignment = new Alignment
    for(n1 <- nv1._2) {
      for(n2 <- tree2.knn(model.getVector(n1), 50)) {
        alignment.add(n1, n2.value, calc(n1, n2.value))
      }
    }
    println("Done aligning")
    alignment
  }


  def showplot(plot: PlotCanvas): Unit = {
    val window = new PlotGUI
    window.contents = swing.Component.wrap(plot)
    window.visible = true
  }


}
