package models

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

sealed class Cluster(point: SpaceTime) {
  val root: ListBuffer[ListBuffer[SpaceTime]] = ListBuffer(ListBuffer(point))

  def exists(cond: SpaceTime => Boolean): Boolean = root.exists((x: ListBuffer[SpaceTime]) => x.exists(cond))

  def ++=(c: Cluster): Cluster = {
    root ++= c.root
    this
  }

  def +=(stCoor: SpaceTime): Cluster = {
    root.head += stCoor
    this
  }

  def toArray: Array[SpaceTime] = {
    @tailrec
    def sum(x: ListBuffer[ListBuffer[SpaceTime]], result: Int = 0): Int = if (x.isEmpty) result else sum(x.tail, result + x.head.size)
    val length = sum(root)

    val iterators = root.map(_.iterator)

    for (t <- Array.range(0, length)) yield {
      val minInd = root.indices.filter(root(_).nonEmpty).minBy(root(_).head.timestamp)
      val element = root(minInd).head
      root(minInd) = root(minInd).tail
      element
    }
  }
}
