package models

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

sealed class Cluster(point: Checkin) {
  val root: ListBuffer[ListBuffer[Checkin]] = ListBuffer[ListBuffer[Checkin]](ListBuffer(point))

  def exists(cond: Checkin => Boolean): Boolean = root.exists((x: ListBuffer[Checkin]) => x.exists(cond))

  def ++=(c: Cluster): Cluster = {
    root ++= c.root
    this
  }

  def +=(stCoor: Checkin): Cluster = {
    root.head += stCoor
    this
  }

  def toArray: Array[Checkin] = {
    @tailrec
    def sum(x: ListBuffer[ListBuffer[Checkin]], result: Int = 0): Int = if (x.isEmpty) result else sum(x.tail, result + x.head.size)
    val length = sum(root)
    val array = new Array[Checkin](length)

    val iterator = new Array[ListBuffer[Checkin]](root.size)
    var i = 0
    for (br <- root) {
      iterator(i) = br
      i += 1
    }

    def getIndex: Int = {
      var init = 0
      var result = 0
      for (i <- iterator.indices) {
        if (iterator(i).nonEmpty && iterator(i).head.timestamp > init){
          init = iterator(i).head.timestamp
          result = i
        }
      }
      return result
    }
    for (n <- array.indices) {
      val itIndex = getIndex
      array(n) = iterator(itIndex).head
      iterator(itIndex) = iterator(itIndex).tail
    }
    return array
  }
}
