package dataset

import java.io.PrintWriter
import scala.io.Source

class TaskPool[T](size: Int, file: String) {
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

//  def main(): Unit = {
//    val workers = new Array[Task](size)
//    val threads = new Array[Thread](size)
//    for (i <- 0 until size) {
//      workers(i) = new T(buffer, i + 1)
//      threads(i) = new Thread(workers(i))
//      threads(i).start()
//    }
//
//    dispatch()
//
//    for (i <- 0 until size) {
//      threads(i).join()
//    }
//    val writer = new PrintWriter(file)
//    writer.println("imei,#check-ins,active time (s),entropy,topSetSize,starting time,ending time,active days,max speed,#abnormal speed,error msg")
//    for (i <- 0 until size) {
//      for (line <- Source.fromFile(workers(i).output).getLines()) {
//        writer.println(line)
//      }
//    }
//    writer.close()
//  }
}
