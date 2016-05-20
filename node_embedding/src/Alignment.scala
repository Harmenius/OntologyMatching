import com.google.common.collect.MinMaxPriorityQueue

import scala.collection.mutable
import math.max

/**
  * Created by harmen on 22-4-16.
  */
class Alignment {
  def add(node1: String, node2: String, d: Double): Unit = {
    if (d.isNaN)
      return
    alignments.add((node1, node2, d))
  }
  val ordering = new Ordering[(String, String, Double)] {
    override def compare(x: (String, String, Double), y: (String, String, Double)): Int = x._3.compare(y._3)
  }
  val alignments = MinMaxPriorityQueue.orderedBy(ordering).maximumSize(20000).create[(String, String, Double)]()
  //TODO: unhardcode maximum size


}
