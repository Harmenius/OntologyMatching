import com.hp.hpl.jena.rdf.model.{RDFNode, Resource, Statement}
import smile.neighbor.KDTree
import smile.plot.PlotCanvas

import scala.collection.JavaConverters._
import math.sqrt
import scala.collection.mutable
import scala.util.Try

object Evaluator {

  val opts  = WordSenseOpts
  val model = new SkipGramNodeEmbedding()

  def compare(alignment: Alignment, truth: Alignment) : Double = {
    val alignmentset = alignment.alignments.asScala.toSet[(String, String, Double)].map{case (s: String, o:String, v: Double) => (s, o)}
    val truthset = truth.alignments.asScala.toSet[(String, String, Double)].map{case (s: String, o: String, v: Any) => (s, o)}
    val N = alignmentset.size
    val M = truthset.size

    printf("N: %d, M: %d%n", N, M)

    val alignmentset_ = alignmentset.map(_.swap)
    val count: Float = ((alignmentset union alignmentset_) intersect truthset).size
    println("Accuracy %f".format(count / N))
    println("Recall %f".format(count / M))
    2f * count / (N + M)
  }

  def obtain_name(subj: String, predicate: String) : String = {
    val (o, uri) = predicate.endsWith("1") match {
      case true  => (model.getOntologies(0), "http://mouse.owl#")
      case false => (model.getOntologies(1), "http://human.owl#")
    }
    o match {
      case ontology: RDFOntology =>
        Try(ontology.get_label(uri + subj)).getOrElse(subj)
      case _ =>
        subj
    }
  }

  def loadTruth(truthfile: String = WordSenseOpts.truthfile.value) : Alignment = {
    val truthontology = new RDFOntology(truthfile, true)
    val truthalignment = new Alignment
    val edgemap = mutable.HashMap[String, (String, String, String)]()
    for (edge <- truthontology.getEdges) {
      //val (obj : String) :: (subj : String) :: _ = edge.split(" ")
      val subj  = edge.split(" ")(0)
      val pred = edge.split(" ")(1)
      val obj = edge.split(" ")(2)
      val subj_  = obtain_name(subj, pred)

      var trip = Try(edgemap(obj)).getOrElse((null, null, null))
      trip = pred.last match {
        case '1' => (subj_, trip._2, trip._3)
        case '2' => (trip._1, subj_, trip._3)
        case 'e' => (trip._1, trip._2, subj_)
      }
      if (trip._1 != null && trip._2 != null && trip._3 != null)
        truthalignment.add(trip._1, trip._2, trip._3.toDouble)
      else
        edgemap.put(obj, trip)
    }
    truthalignment
  }

  def main(args: Array[String]) {
    println("Starting")
    val alignment = makeAlignment()
    alignment.set_threshold(0.04)
    val truth = loadTruth()
    //Visualizer.hist_alignment(alignment, truth) // Only visualize if alignment contains all pairs
    Visualizer.compare_hist(alignment, truth)
    val dice = compare(alignment, truth)
    val synonyms = loadTruth(WordSenseOpts.synonyms.value)
    val dice_ = compare(synonyms, truth)
    printf("The result of what you have been working for for months: %s%n", dice)
    printf("For reference, just the synonyms scores: %s%n", dice_)
  }

  class PlotGUI extends swing.MainFrame {
    title = "Plot"
    preferredSize = new swing.Dimension(500, 500)
  }

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
