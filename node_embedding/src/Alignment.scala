import com.google.common.collect.MinMaxPriorityQueue
import collection.JavaConversions._

/**
  * Created by harmen on 22-4-16.
  */
class Alignment {
  var t: Double = Double.PositiveInfinity

  def add(node1: String, node2: String, d: Double) = {
    if (!d.isNaN)
      if (d < t)
        alignments.add((node1, node2, d))
  }

  def set_threshold(new_t: Double, mustbehigher: Boolean = false) = {
    if (new_t < 1.0) {
      t = new_t
      alignments.retainAll(alignments.filter(tup => (tup._3 >= t) == mustbehigher))
    } else {
      val t_ : Int = new_t.toInt
      if (t_ < alignments.size) {
        val alignments_ = MinMaxPriorityQueue.orderedBy(ordering).maximumSize(t_).create[(String, String, Double)]()
        alignments_.addAll(this.alignments.take(t_)) //.dropRight(alignments.size() - t_)
        alignments = alignments_
      }
    }
  }

  val ordering = new Ordering[(String, String, Double)] {
    override def compare(x: (String, String, Double), y: (String, String, Double)): Int = x._3.compare(y._3)
  }
  var alignments = MinMaxPriorityQueue.orderedBy(ordering).maximumSize(100000).create[(String, String, Double)]()
  //TODO: unhardcode maximum size


}
