
import com.alibaba.fastjson.JSON

import java.io.{File, PrintWriter}
import models._
import utils.{JsonTrace, _}

import scala.io.Source

object display {
  def main(args: Array[String]): Unit = {
//    val devID = args(0)
    val devID = "75ae553f1fff186af4d28b5cd9d9844c"
    for (id <- 0 until 10) {
      for (line <- Source.fromFile("tracedata/rtbasia_trace.shanghai.part%d.json".format(id)).getLines()) {
        val traceJOb = JSON.parseObject(line)
        val imei = traceJOb.getString("did")
        if (imei == devID) {
          val traceJSONArray = traceJOb.getJSONArray("traces")
          val trace = JsonTrace.cons(traceJSONArray)
          val locProfile = new LocationProfile(trace).locations
          val night_top = locProfile.indices.maxBy(i => locProfile(i).checkins.count(l => l.timestamp % (24 * 3600) >= 16 * 3600 && l.timestamp % (24 * 3600) < 23 * 3600))
          println(night_top)
          val writer = new PrintWriter(new File("display top/trace.js"))
          var traceText: String = ""
          writer.println("var geo = new Array()")
          writer.println("var assoc")
          for (i <- locProfile.indices) {
            writer.println("assoc = new Array()")
            val loc = locProfile(i)
            for (j <- loc.checkins.indices) {
              val gcj02_coor = loc.checkins(j).wgs84_to_gcj02
              writer.println("assoc[%d]=new SpaTem(%.6f,%.6f,%d)".format(j, gcj02_coor.latitude, gcj02_coor.longitude, loc.checkins(j).timestamp))
            }
            val gcj02_coor = loc.wgs84_to_gcj02
            writer.println("geo[%d]=new Location(%.6f,%.6f,assoc,%d)".format(i, gcj02_coor.latitude, gcj02_coor.longitude, loc.checkins.length))//loc.timeSpan))
          }
          writer.close()
        }
      }
    }
  }
}
