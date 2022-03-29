package dataset

class TraceBuffer(size: Int) {
  private val traceData = new Array[String](size)
  private var len = 0
  private var empty = true
  private var closed = false

  def input(inputData: Array[String], length: Int): Unit = this.synchronized {
    while (!empty) {
      wait()
    }
    for (i <- inputData.indices) {
      traceData(i) = inputData(i)
    }
    len = length
    empty = false
    notifyAll()
  }

  def get(outputData: Array[String]): Int = this.synchronized {
    while (empty && !closed) {
      wait()
    }
    for (i <- 0 until len) {
      outputData(i) = traceData(i)
    }
    val ret_len = len
    len = 0
    empty = true
    notifyAll()
    return ret_len
  }

  def close(): Unit = {
    closed = true
  }
}
