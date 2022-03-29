import com.alibaba.fastjson.JSON
import dataset.{Dispatcher, Task, TraceBuffer}
import models.{Location, LocationProfile}
import utils.JsonTrace

import java.io.PrintWriter
import scala.io.Source

object mobiPattern {
  val poolsize = 20
  val dispatcher = new Dispatcher

  def main(args: Array[String]): Unit = {
    val workers = new Array[mobiPattern](poolsize)
    val threads = new Array[Thread](poolsize)
    for (i <- 0 until poolsize) {
      workers(i) = new mobiPattern(dispatcher.buffer, i + 1)
      threads(i) = new Thread(workers(i))
      threads(i).start()
    }

    dispatcher.dispatch()

    for (i <- 0 until poolsize) {
      threads(i).join()
    }
    val writer = new PrintWriter("mobiPattern.csv")
//    writer.println("imei,#check-ins,active time (s),entropy,topSetSize,starting time,ending time,active days,max speed,#abnormal speed,error msg")
    for (i <- 0 until poolsize) {
      for (line <- Source.fromFile(workers(i).tempOut).getLines()) {
        writer.println(line)
      }
    }
    writer.close()
  }
}

class mobiPattern(buffer: TraceBuffer, id: Int) extends Task(buffer, id) {
  def countActiveDays(location: Location): Int ={
    var timestamp = location.checkins(0).timestamp
    var last_day = (timestamp + 14400) / 24 / 3600
    var activeDays = 1
    for (c <- location.checkins) {
      if (c.timestamp < timestamp) errLog.println("timestamp order error, %d, %d".format(c.timestamp, timestamp))
      val date = (c.timestamp + 14400) / 24 / 3600
      if (date > last_day) activeDays += 1
      last_day = date
      timestamp = c.timestamp
    }
    return activeDays
  }

  override def mainTask(traceData: String): Unit = {
    val traceJOb = JSON.parseObject(traceData)
    val imei = traceJOb.getString("did")
    val traceJSONArray = traceJOb.getJSONArray("traces")
    val checkins = JsonTrace.cons(traceJSONArray)
    val profile = new LocationProfile(checkins)
    val locations = profile.locations
    val trace = profile.trace
    if (locations.length < 4) return//errLog.println("%s with too few locations, %d.".format(imei, locations.length))
    else {
      val top1 = locations(0)
      val top2 = locations(1)
      var timestamp = trace(0).timestamp
      var last_day = (timestamp + 14400) / 24 / 3600
      var activeDays = 1
      var top1exists = false
      var top2exists = false
      for (c <- trace) {
        if (c.timestamp < timestamp) errLog.println("%s timestamp order error, %d, %d".format(imei, c.timestamp, timestamp))
        val date = (c.timestamp + 14400) / 24 / 3600
        if (date > last_day) {
          if (top1exists && top2exists) activeDays += 1
          top1exists = false
          top2exists = false
        }
        if (c.location == top1) top1exists = true
        else if (c.location == top2) top2exists = true
        last_day = date
        timestamp = c.timestamp
      }
      if (top1exists && top2exists) activeDays += 1
      val numtops = locations.count(_.checkins.length > 10)
      val activeLocations = locations.count(countActiveDays(_) > 10)
      writeln("%s,%d,%d,%d,%d,%d".format(imei, numtops, locations(0).checkins.length, locations(1).checkins.length, activeDays, activeLocations))
    }
  }
}