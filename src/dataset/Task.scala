package dataset

import utils.ErrorLog

import java.io.PrintWriter
import java.text.SimpleDateFormat

abstract class Task(buffer: TraceBuffer, id: Int) extends Runnable {
  val errLog = new ErrorLog(getClass.getName)

  val tempOut: String = "tmp/shanghai-dev-%d.csv".format(id)
  private val writer = new PrintWriter(tempOut)
  def writeln(x: Any): Unit = writer.println(x)

  def mainTask(traceData: String): Unit

  override def run(): Unit = {
    val out = new Array[String](5)
    var cnt = 0
    var len = buffer.get(out)
    while (len != 0) {
      cnt += len
      val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
      println(current)
      println("\tThread %s\t%d".format(id, cnt))
      for (i <- 0 until len) {
        val traceData = out(i)
        mainTask(traceData)
      }
      len = buffer.get(out)
    }
    writer.close()
  }
}
