/**
  * Created by harmen on 8-6-16.
  */

import scala.collection.JavaConverters._
import breeze.linalg._
import breeze.plot

object Visualizer {
  def hist_alignment(alignment: Alignment, truth: Alignment) = {
    val values: Array[Double] = alignment.alignments.asScala.toArray.map(_._3).sorted
    val vec = DenseVector(values)

    val values_ : Array[Double] = truth.alignments.asScala.toArray.map(_._3).sorted
    val vec_ = DenseVector(values_)
    println(vec_)

    val f = plot.Figure()
    val p = f.subplot(0)
    p += plot.hist(vec, 80)
    p += plot.hist(vec_, 80)

    p.xlabel = "dist"
    p.ylabel = "#alignments"
    f.saveas("alignment_hist.pdf")
  }

}


class Visualizer {

}
