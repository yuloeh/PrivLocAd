package dataset

import scala.io.Source

class Dispatcher {
  val buffer = new TraceBuffer(5)

  def dispatch(): Unit = {
    val inp = new Array[String](5)
    var i = 0
    for (id <- 0 until 10) {
      for (line <- Source.fromFile("tracedata/rtbasia_trace.shanghai.part%d.json".format(id)).getLines()) {
        inp(i) = line
        i += 1
        if (i == 5) {
          buffer.input(inp, 5)
          i = 0
        }
      }
    }
    if (i != 0)
      buffer.input(inp, i)

    buffer.close()
  }
}
