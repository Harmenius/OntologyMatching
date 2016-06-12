/**
  * Created by harmen on 8-6-16.
  */

import scala.collection.JavaConverters._
import breeze.linalg._
import breeze.plot

import scala.collection.immutable.HashMap

object Visualizer {
  def hist_alignment(alignment: Alignment) = {
    val keys = alignment.alignments.asScala.toArray.map{case (a,b,c) => (a,b)}
    val values: Array[Double] = alignment.alignments.asScala.toArray.map(_._3).sorted

    val alignmentMap = (keys zip values).toMap
    val vec = DenseVector(values)

    val f = plot.Figure()
    val p = f.subplot(0)
    p += plot.hist(vec, 80)

    p.xlabel = "dist"
    p.ylabel = "#alignments"
    f.saveas("alignment_hist.pdf")
  }

  def compare_hist(alignment: Alignment, truth: Alignment) = {
    val keys = alignment.alignments.asScala.toArray.map{case (a,b,c) => (a,b)}
    val values: Array[Double] = alignment.alignments.asScala.toArray.map(_._3).sorted

    val alignmentMap = (keys zip values).toMap
    val vec = DenseVector(values)

    val keys_ = truth.alignments.asScala.toArray.map{case (a,b,c) => (a,b)}
    val values_ : Array[Double] = keys_.map(t => Evaluator.calc(t._1, t._2))
    val vec_ = DenseVector(values_)

    val f = plot.Figure()
    val p = f.subplot(0)
    p += plot.hist(vec, 80)
    p += plot.hist(vec_, 80)

    p.xlabel = "dist"
    p.ylabel = "#alignments"
    f.saveas("comparative_hist.pdf")
  }

}
