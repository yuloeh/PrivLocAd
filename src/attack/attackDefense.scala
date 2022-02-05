package attack

import java.io.{FileWriter, PrintWriter}
import com.alibaba.fastjson.{JSON, JSONObject}
import distributions.NormalDistribution
import models.{Checkin, Coordinate, LocationProfile, LocationProfileExt}
import privacy.{ln_100, ln_20}
import utils.{ErrorLog, JsonTrace, TraceBuffer}

import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math.{Pi, cos, log, sin, sqrt}
import scala.util.Random

class attackDefense(buffer: TraceBuffer, id: Int) extends Runnable {
  val outFile: String = "temp/attack-%d.csv".format(id)
  val errLog = new ErrorLog(getClass.getName)

//  val epsilon = 1
  val r_ind = 500
  private def sigma(k: Int, epsilon: Double) = sqrt(k * (2 * ln_100 + epsilon)) * r_ind / epsilon

  private def obfuscate(point: Coordinate, distribution: NormalDistribution): Array[Checkin] = {
    val listBuffer = new ListBuffer[Checkin]
    for (i <- 0 until 10) {
      val (x, y) = distribution.sample
      val obfuscated = privacy.obfuscate(point, x, y)
      listBuffer += Checkin(obfuscated.latitude, obfuscated.longitude, 0)
    }
    return listBuffer.toArray
  }

  override def run(): Unit = {
    val writer = new PrintWriter(new FileWriter(outFile))
    val out = new Array[String](5)
    var cnt = 0
    while (true) {
      val len = buffer.get(out)
      if (len == 0) {
        writer.close()
        return
      }
      cnt += len
      val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
      println(current)
      println("\tThread %s\t%d".format(id, cnt))

      for (i <- 0 until len) {
        val traceJOb = JSON.parseObject(out(i))
        val imei = traceJOb.getString("did")
        val traceJSONArray = traceJOb.getJSONArray("traces")
        val rawTrace = JsonTrace.cons(traceJSONArray)
        val locProf = new LocationProfileExt(rawTrace)
        val locations = locProf.locations
        val trace = locProf.trace

        if (locations.length >= 2) {
          val total = locations.map(_.timeSpan).sum
          writer.print("%s,%d".format(imei, total))

          val r = 200 // 200m as in ccs'13
          for (epsilon <- Array(1.0, 1.5)) {
            val Sigma = sigma(10, epsilon)
            val distribution = new NormalDistribution(Sigma)
            val top1obfus = obfuscate(locations(0), distribution)
            val top2obfus = obfuscate(locations(1), distribution)
            val obfTrace = trace.map(x =>
              if (x.location == locations(0)) {
//                println("yes")
                val obs = top1obfus(Random.nextInt(10))
                Checkin(obs.latitude, obs.longitude, x.timestamp)
              } else if (x.location == locations(1)) {
//                println("yes")
                val obs = top2obfus(Random.nextInt(10))
                Checkin(obs.latitude, obs.longitude, x.timestamp)
              } else Checkin(x.latitude, x.longitude, x.timestamp)
            )
            val clusterRad = sqrt(2 * ln_20) * Sigma

            val top = deobfuscate(obfTrace, clusterRad)
            val dist = locations(0).distance(top)

            val remTrace = obfTrace.filter(p => !top.checkins.map(_.asInstanceOf[Checkin]).contains(p))
            if (remTrace.length == 0) {
              writer.print(",%f,NULL,%f,0".format(dist, dist))
            }
            else {
              val top2 = deobfuscate(remTrace, clusterRad)

              val accError1 = locations(0).distance(top) + locations(1).distance(top2)
              val accError2 = locations(0).distance(top2) + locations(1).distance(top)
              if (accError1 < accError2) {
                writer.print(",%f,%f,%f,0".format(locations(0).distance(top), locations(1).distance(top2), accError1 / 2))
              }
              else {
                writer.print(",%f,%f,%f,1".format(locations(0).distance(top2), locations(1).distance(top), accError2 / 2))
              }
            }
          }
          writer.println()
        }
        else {
          errLog.println(imei + "location number < 2")
        }
      }
    }
    writer.close()
  }
}

object attackDefense {
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

  def main(args: Array[String]): Unit = {
    val poolSize = 20
    val workers = new Array[attackDefense](poolSize)
    val threads = new Array[Thread](poolSize)
    for (i <- 0 until poolSize) {
      workers(i) = new attackDefense(buffer, i + 1)
      threads(i) = new Thread(workers(i))
      threads(i).start()
    }

    dispatch()

    for (i <- 0 until poolSize) {
      threads(i).join()
    }
    val writer = new PrintWriter("attack-defense.csv")
    writer.println("imei,active time (s),top-1 distance,top-2 distance,average,flag,top-1 distance,top-2 distance,average,flag")
    for (i <- 0 until poolSize) {
      for (line <- Source.fromFile(workers(i).outFile).getLines()) {
        writer.println(line)
      }
    }
    writer.close()
  }
}
