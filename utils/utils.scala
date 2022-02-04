import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

package object utils {
  def first[T](array: Array[T], cond: T => Boolean): Int = __first(array, cond, 0, array.length - 1)
  def last[T](array: Array[T], cond: T => Boolean): Int = __last(array, cond, 0, array.length - 1)

/*  def aggregate[T](array: Array[T], mapping: T => Int = (x: Int) => x): Int = {
    def __aggregate(index: Int = 0, retVal: Int = 0): Int = if (index != array.length) __aggregate(index + 1, retVal + mapping(array(index))) else retVal
    __aggregate(0)
  }

  def aggregate[T](array: Array[T], mapping: T => Double = (x: Double) => x): Double = {
    def __aggregate(index: Int = 0, retVal: Double = 0): Double = if (index != array.length) __aggregate(index + 1, retVal + mapping(array(index))) else retVal
    __aggregate(0)
  }
*/
  def quickSort[T](array: Array[T], comp: (T, T) => Boolean): Unit = {
    __quickSort(0, array.length - 1)
    def __quickSort(start: Int, end: Int): Unit = {
      if (start >= end) return

      def less_than(a: T): T => Boolean = (x: T) => comp(x, a)

      def greater_than(a: T): T => Boolean = (x: T) => comp(a, x)

      val pivot = array(end)

      @tailrec
      def partition(low: Int, high: Int): Int = {
        //println(low, high, array(low), array(high - 1), array(high))
        val i = __first(array, greater_than(pivot), low, high - 1)
        if (i == high) return high
        array(high) = array(i)
        val j = __last(array, less_than(pivot), i, high - 1)
        if (i >= j) return i
        array(i) = array(j)
        return partition(i, j)
      }

      val p = partition(start, end)
      array(p) = pivot

      __quickSort(start, p - 1)
      __quickSort(p + 1, end)
    }
  }

  @tailrec
  private def __first[T](array: Array[T], cond: T => Boolean, start: Int, end: Int): Int = if (start <= end) {
    if (cond(array(start))) start else __first(array, cond, start + 1, end)
  } else start

  @tailrec
  private def __last[T](array: Array[T], cond: T => Boolean, start: Int, end: Int): Int = if (start <= end) {
    if (cond(array(end))) end else __last(array, cond, start, end - 1)
  } else end

  def power(base: Double, exp: Int): Double = {
    @tailrec
    def __pow(base: Double, exp: Int, res: Double = 1.0): Double =
      if (exp == 1) base * res else if (exp % 2 == 1) __pow(base * base, exp / 2, base * res) else __pow(base * base, exp / 2, res)
    if (exp == 0) {
      if (base == 0) throw new IllegalArgumentException
      else 1
    }
    else if (exp > 0) __pow(base, exp) else 1 / __pow(base, -exp)
  }

  val shanghai_polygon = Array((121.42,31.47), (121.16,31.37), (121.2432,31.16572), (121.37,30.98), (121.513509,31.0175), (121.72,31.16), (121.6872,31.3176))
  def inPolygon (vertices: Array[Tuple2[Double, Double]], point: Tuple2[Double, Double]): Boolean = {
    var start = vertices(vertices.length - 1)
    for (end <- vertices) {
      if ((start._1 - point._1) * (end._2 - start._2) - (end._1 - start._1) * (start._2 - point._2) < 0)
        return false
      start = end
    }
    return true
  }
}
