package utils

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat

class ErrorLog(val module: String) {
  def println(message: String): Unit = {
    val writer = new PrintWriter(new FileWriter("err.log", true))
    val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
    writer.println(current)
    writer.println("In %s:".format(module))
    writer.println(message)
    writer.close()
  }
}
