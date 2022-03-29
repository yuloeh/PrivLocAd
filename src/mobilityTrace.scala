import com.alibaba.fastjson.JSON
import models.Coordinate

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object mobilityTrace {
  def main(args: Array[String]): Unit = {
    val devID = "75ae553f1fff186af4d28b5cd9d9844c"
    for (id <- 0 until 10) {
      for (line <- Source.fromFile("tracedata/rtbasia_trace.shanghai.part%d.json".format(id)).getLines()) {
        val traceJOb = JSON.parseObject(line)
        val imei = traceJOb.getString("did")
        if (imei == devID) {
          val traceJSON = traceJOb.getJSONArray("traces")
          val writer = new PrintWriter(new File("trace.js"))
          writer.print("var jsonData = '{\"did\":\"%s\",\"traces\":[".format(devID))
          var flag_comma = false
          for (i <- 0 until traceJSON.size()) {
            val node = traceJSON.getJSONObject(i)
            val tim = node.getIntValue("timestamp")
            if (true) {
              val lat = node.getDouble("latitude")
              val lon = node.getDouble("longitude")
              val coor = new Coordinate(lat, lon).wgs84_to_gcj02
              if (flag_comma) writer.print(",")
              else flag_comma = true
              writer.print("{\"latitude\":%.6f,\"longitude\":%.6f,\"timestamp\":%d}".format(coor.latitude, coor.longitude, tim))
            }
          }
          writer.println("]}'")
          writer.close()
        }
      }
    }
  }
}
