import com.alibaba.fastjson.JSON
import models.LocationProfile
import utils.JsonTrace

import java.io.{File, PrintWriter}
import scala.io.Source

object displayMobi {

  def main(args: Array[String]): Unit = {
    val devID = args(0)
    for (id <- 0 until 10) {
      for (line <- Source.fromFile("tracedata/rtbasia_trace.shanghai.part%d.json".format(id)).getLines()) {
        val traceJOb = JSON.parseObject(line)
        val imei = traceJOb.getString("did")
        if (imei == devID) {
          val traceJSONArray = traceJOb.getJSONArray("traces")
          val checkins = JsonTrace.cons(traceJSONArray)
          val profile = new LocationProfile(checkins)
          val locations = profile.locations
          val trace = profile.trace
          val top1 = locations(0)
          val top2 = locations(1)
          var timestamp = trace(0).timestamp
          var date = (timestamp + 14400) / 24 / 3600
          var activeDays = 1
          var top1exists = false
          var top2exists = false
          for (c <- trace) {
            val day = (c.timestamp + 14400) / 24 / 3600
            if (day > date) {
              if (top1exists && top2exists) activeDays += 1
              top1exists = false
              top2exists = false
            }
            if (c.location == top1) top1exists = true
            else if (c.location == top2) top2exists = true
            date = day
            timestamp = c.timestamp
          }

//          val night_top = locProfile.indices.maxBy(i => locProfile(i).assoc.count(l => l.timestamp % (24 * 3600) >= 16 * 3600 && l.timestamp % (24 * 3600) < 23 * 3600))
//          println(night_top)
//          val writer = new PrintWriter(new File("display top/trace.js"))
//          var traceText: String = ""
//          writer.println("var geo = new Array()")
//          writer.println("var assoc")
//          for (i <- locProfile.indices) {
//            writer.println("assoc = new Array()")
//            val loc = locProfile(i)
//            for (j <- loc.assoc.indices) {
//              val gcj02_coor = wgs84_to_gcj02(loc.assoc(j))
//              writer.println("assoc[%d]=new SpaTem(%.6f,%.6f,%d)".format(j, gcj02_coor.getLatitude, gcj02_coor.getLongitude, loc.assoc(j).timestamp))
//            }
//            val gcj02_coor = wgs84_to_gcj02(loc)
//            writer.println("geo[%d]=new Location(%.6f,%.6f,assoc,%d)".format(i, gcj02_coor.getLatitude, gcj02_coor.getLongitude, loc.assoc.length))//loc.timeSpan))
//          }
//          writer.close()
        }
      }
    }
  }
}
