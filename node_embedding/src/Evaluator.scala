import java.io.{File, FileReader}

import breeze.io.TextWriter.FileWriter
import com.hp.hpl.jena.rdf.model.{RDFNode, Resource, Statement}
import smile.neighbor.KDTree
import smile.plot.PlotCanvas

import scala.collection.JavaConverters._
import math.sqrt
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Evaluator {

  val opts = WordSenseOpts
  var model: NodeEmbeddingModel = null

  def get_compare_stats(alignment: Alignment, truth: Alignment, calc: (String, String) => Double = this.calc): (Int, Int, Int) = {
    val alignmentset = alignment.alignments.asScala.toSet[(String, String, Double)].map { case (s: String, o: String, v: Double) => (s, o) }
    val truthset = truth.alignments.asScala.toSet[(String, String, Double)].map { case (s: String, o: String, v: Any) => (s, o) }
    val truthset_ = truthset.filterNot(t => calc(t._1, t._2) >= 100000)
    val N = alignmentset.size
    val M = truthset_.size

    val alignmentset_ = alignmentset.map(_.swap)
    val count: Int = ((alignmentset union alignmentset_) intersect truthset_).size
    (N, M, count)
  }

  def compare(alignment: Alignment, truth: Alignment): Double = {
    val stats = get_compare_stats(alignment, truth)
    val N = stats._1
    val M = stats._2
    val count = stats._3.toFloat
    println("Accuracy %f".format(count / N))
    println("Recall %f".format(count / M))
    2f * count/ (N + M)
  }

  def show_alignment(alignment: Alignment, other: Alignment) = {
    val a = alignment.alignments.asScala.toSet[(String, String, Double)]
    val a_ = other.alignments.asScala.toSet[(String, String, Double)]
    val b = a.flatMap(t => Array(t._1, t._2))
    val b_ = a_.flatMap(t => Array(t._1, t._2))
    val minus = a_ -- a//a -- a_
    val minus_ = minus.filter(t => t._1 != t._2)
    //val c = b.map(model.getVocab.getId(_))
    println(minus_)
  }

  def obtain_name(subj: String, predicate: String) : String = {
    val (o, uri) = predicate.endsWith("1") match {
      case true  => (model.getOntologies(0), "http://" + WordSenseOpts.corpusses.value.split(";").head.split("/").last + "#")
      case false => (model.getOntologies(1), "http://" + WordSenseOpts.corpusses.value.split(";")(1).split("/").last + "#")
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
        truthalignment.add(trip._1.toLowerCase, trip._2.toLowerCase, trip._3.toDouble)
      else
        edgemap.put(obj, trip)
    }
    truthalignment
  }

  def plot_roc(alignment: Alignment, truth: Alignment, calc: (String, String) => Double = this.calc): Unit = {
    val ps = new ArrayBuffer[Double]()
    val rs = new ArrayBuffer[Double]()
    var i = 0
    var correct = 0
    val truthset_ = truth.alignments.asScala.map(t => (t._1, t._2)).toSet
    val truthset = truthset_.filterNot(t => calc(t._1, t._2) >= 100000)
    val N = truthset.size
    for (trip <- alignment.alignments.asScala.toList.sortWith((t, t_) => t._3 < t_._3)) {
      i += 1
      if (truthset.contains((trip._1, trip._2)) || truthset.contains((trip._2, trip._1)))
        correct += 1
      ps += correct.toFloat/i
      rs += correct.toFloat/N
    }

    //var mx = 0.toDouble
    //val ps_ = ps.map(v => {mx = math.max(mx, v); mx})
    val fs = ps zip rs map {case (p, r) => 2*p*r / (p + r)}
    val order = fs.zipWithIndex.sortWith((t, t_) => t._1 < t_._1).map(t => t._2).reverse
    val sorted = order.map(fs(_))
    println(sorted.slice(0,10) zip order.map(rs(_)))

    Visualizer.scatter(rs, ps)
    val cutoff = ps.size
    println("AP: " + ps.slice(0, cutoff).sum/cutoff)
    println("F: " + fs.max)
  }

  def main(args: Array[String]) {
    model = new DeepWalkNodeEmbedding//new SkipGramNodeEmbedding()
    val write = false
    if (write) {
      val o1 = new RDFOntology(WordSenseOpts.corpusses.value.split(";").head)
      val o2 = new RDFOntology(WordSenseOpts.corpusses.value.split(";").last)

      val e1 = o1.getEdges.map(s => s.split(" ")(0) + " " + s.split(" ")(2))
      val e2 = o2.getEdges.map(s => s.split(" ")(0) + " " + s.split(" ")(2))
      val e1_ = o1.getEdges.map(s => s.split(" ")(2) + " " + s.split(" ")(0))
      val e2_ = o2.getEdges.map(s => s.split(" ")(2) + " " + s.split(" ")(0))

      val es = e1 ++ e2 ++ e1_ ++ e2_

      val file = new FileWriter(new File("edges.edgelist"))
      val hm = new mutable.HashMap[String, Int]()
      for (e <- es) {
        var e_str = ""
        for (n <- e.split(" ")) {
          if (!hm.contains(n)) {
            hm.put(n, hm.size)
          }
          e_str += hm(n).toString + " "
        }
        file.append(e_str.trim + "\n")
      }
      file.close()

      val file_ = new FileWriter(new File("edgemappings.csv"))
      for (pair <- hm.toList) {
        file_.append(pair._1.toString + " " + pair._2.toString + "\n")
      }
      file_.close()
    } else {
      println("Starting")

      val file = io.Source.fromFile("edgemappings.csv")
      val hm = new mutable.HashMap[String, String]()
      for (line <- file.getLines()) {
        hm.put(line.split(" ").head, line.split(" ").last)
      }

      //val truth = loadTruth()

      //model.buildVocab()
      //model.learnEmbeddings()
      //model.loadEmbeddings(hm)

      val alignment = makeAlignment(alreadyloaded = false)
      dotherest(alignment)
    }
  }

    def dotherest(alignment: Alignment) {
      //Visualizer.hist_alignment(alignment)
      alignment.set_threshold(-0.0075)
      val truth = loadTruth()
      //Visualizer.compare_hist(alignment, truth)
      val dice = compare(alignment, truth)
      //val synonyms = loadTruth(WordSenseOpts.synonyms.value)
      //val dice_ = compare(synonyms, truth)
      printf("The result of what you have been working for for months: %s%n", dice)
      //printf("For reference, just the synonyms scores: %s%n", dice_)
      show_alignment(alignment, truth)
      plot_roc(alignment, truth)
    }

    /*
    println("Starting")
    val alignment = makeAlignment()
    Visualizer.hist_alignment(alignment)
    alignment.set_threshold(0.04)
    val truth = loadTruth()
    Visualizer.compare_hist(alignment, truth)
    val dice = compare(alignment, truth)
    val synonyms = loadTruth(WordSenseOpts.synonyms.value)
    val dice_ = compare(synonyms, truth)
    printf("The result of what you have been working for for months: %s%n", dice)
    printf("For reference, just the synonyms scores: %s%n", dice_)
    //show_alignment(alignment, truth)
    //plot_roc(alignment, truth)
    */

  class PlotGUI extends swing.MainFrame {
    title = "Plot"
    preferredSize = new swing.Dimension(500, 500)
  }

  def calc(node1: String, node2: String): Double = {
    if (node1 == node2)
      return -0.1
    val v1 = model.asInstanceOf[SkipGramNodeEmbedding].getVector(node1)
    val v2 = model.asInstanceOf[SkipGramNodeEmbedding].getVector(node2)
    val v = sqrt((v1 zip v2 map {case (x,y)=> (x - y) * (x - y)}).sum) // Euclidean vector distance
    if (v.isNaN) {
      100000d
    }
    else
      v
  }

  object KDTree {
    def apply(pair: (Array[Array[Double]], Array[String])) : KDTree[String] = new KDTree(pair._1, pair._2)
  }

  def makeAlignment(alreadyloaded: Boolean = false) : Alignment = {
    if (!alreadyloaded) {
      model.buildVocab(false)
      model.learnEmbeddings()
    }

    val o1 = model.getOntologies(0)
    val o2 = model.getOntologies(1)
    println("Building trees for aligning")
    val synonyms = new mutable.HashMap[String, String]()// model.loadSynonyms()

    val nodes1 = o1.getNodes.map(model.getRootSynonym(_, synonyms)).toSet[String].toArray[String] // Don't grab all synonyms too
    val vectors1 : Array[Array[Double]] = nodes1.map(n => model.asInstanceOf[SkipGramNodeEmbedding].getVector(n))
    val nv1 = vectors1.zip(nodes1).filterNot{case (v,_) => v(0).isNaN}.unzip

    val nodes2 = o2.getNodes.map(model.getRootSynonym(_, synonyms)).toSet[String].toArray[String]
    val vectors2 : Array[Array[Double]] = nodes2.map(n => model.asInstanceOf[SkipGramNodeEmbedding].getVector(n))
    val nv2 = vectors2.zip(nodes2).filterNot{case (v,_) => v(0).isNaN}.unzip

    val tree1 = KDTree(nv1)
    val tree2 = KDTree(nv2)
    println("Trees built")

    println("Starting with aligning")
    val alignment = new Alignment
    for(n1 <- nv1._2) {
      for(n2 <- tree2.knn(model.asInstanceOf[SkipGramNodeEmbedding].getVector(n1), 2)) {
        val n2_ = n2.value
        if (!n1.contains("blank") && !n2_.contains("blank"))
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
