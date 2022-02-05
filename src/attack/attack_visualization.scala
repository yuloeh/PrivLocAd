package attack

import com.alibaba.fastjson.JSON
import distributions.LaplaceDistribution
import models.{Coordinate, Location, LocationProfile}
import utils.JsonTrace

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.math.log

object attack_visualization {
  def main(args: Array[String]): Unit = {
    val line = Source.fromFile("dev-trace.json").getLines().toList.head
    val traceJOb = JSON.parseObject(line)
    val traceJSONArray = traceJOb.getJSONArray("traces")

    val startTime = 1560000000
    val trace = JsonTrace.cons(traceJSONArray, _.getIntValue("timestamp") > startTime)
    val locProfile = new LocationProfile(trace).locations
    val top1 = locProfile(0)
    val b = 200 / log(2)
    val distr = LaplaceDistribution.sample(b)

    val l = log(2)
    val r = 200
    val scale = r / l
    val clusterRad = 4.75 * scale
    val obfTrace = trace.map(x => obfuscate(x, scale))
    val top = deobfuscate(obfTrace, clusterRad)
    val obfuscated = top.checkins

//    def obfuscate(point: Coordinate) = {
//      val cartesian = wsg84_to_cartesian(point)
//      val (dx, dy) = distr.sample
//      cartesian_to_wsg84(cartesian.x + dx, cartesian.y + dy)
//    }

//    val obfuscated = top1.assoc.map(p => obfuscate(p))
    val center_lat = obfuscated.map(_.latitude).sum / obfuscated.length
    val center_lon = obfuscated.map(_.longitude).sum / obfuscated.length
    val center = new Coordinate(center_lat, center_lon)
    //quickSort(locProfile, (x: Location, y: Location) => x.timeSpan > y.timeSpan)
    //quickSort(locProfile, (x: LocProfile, y: LocProfile) => x.location.assoc.length > y.location.assoc.length)
    val writer = new PrintWriter(new File("display top/rawcoor.js"))
    writer.println("var geo = new Array()")
    writer.println("var assoc")
    writer.println("assoc = new Array()")
    for (j <- top1.checkins.indices) {
      val gcj02_coor =top1.checkins(j).wgs84_to_gcj02
      writer.println("assoc[%d]=new SpaTem(%.6f,%.6f,%d)".format(j, gcj02_coor.latitude, gcj02_coor.longitude, top1.checkins(j).timestamp))
    }
    val gcj02_coor = top1.wgs84_to_gcj02
    writer.println("geo[0]=new Location(%.6f,%.6f,assoc,%d)".format(gcj02_coor.latitude, gcj02_coor.longitude, top1.checkins.length))
    writer.close()

    val writer1 = new PrintWriter(new File("display top/obfcoor.js"))
    writer1.println("var geo_obf = new Array()")
    writer1.println("var assoc")
    writer1.println("assoc = new Array()")
    for (j <- obfTrace.indices) {
      val gcj02_coor = obfTrace(j).wgs84_to_gcj02
      writer1.println("assoc[%d]=new SpaTem(%.6f,%.6f,%d)".format(j, gcj02_coor.latitude, gcj02_coor.longitude, obfTrace(j).timestamp))
    }
    val gcj02_center1 = center.wgs84_to_gcj02
    writer1.println("geo_obf[0]=new Location(%.6f,%.6f,assoc,%d)".format(gcj02_center1.latitude, gcj02_center1.longitude, obfTrace.length))
    writer1.close()

    val partObfTrace_1 = obfTrace.filter(_.timestamp < startTime + 7 * 24 * 3600)
    val top_deob_1 = deobfuscate(partObfTrace_1, clusterRad)
    write("display top/obfcoor-week.js", top_deob_1)

    val partObfTrace_2 = obfTrace.filter(_.timestamp < startTime + 30 * 24 * 3600)
    val top_deob_2 = deobfuscate(partObfTrace_2, clusterRad)
    write("display top/obfcoor-month.js", top_deob_2)
  }

  def write(fileName: String, location: Location): Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.println("var geo = new Array()")
    writer.println("var assoc")
    writer.println("assoc = new Array()")
    for (i <- location.checkins.indices) {
      val gcj02_coor = location.checkins(i).wgs84_to_gcj02
      writer.println("assoc[%d]=new SpaTem(%.6f,%.6f,%d)".format(i, gcj02_coor.latitude, gcj02_coor.longitude, 0))
    }
    val gcj02_center2 = location.wgs84_to_gcj02
    writer.println("geo[0]=new Location(%.6f,%.6f,assoc,%d)".format(gcj02_center2.latitude, gcj02_center2.longitude, location.checkins.length))
    writer.close()
  }
}
