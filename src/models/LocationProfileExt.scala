package models

import utils.quickSort

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math.min

class LocationProfileExt(rawTrace: Array[Checkin]) extends Profile {

  private val maxTimeInterval = 2 * 60 // default active time 10 minutes

  val (trace, locations) = {
    val clusters = new ListBuffer[Cluster]

    for (point <- rawTrace) {
      val nearby = clusters.filter((x: Cluster) => x.exists((y: Checkin) => point.distance(y) < connectivity))
      if (nearby.isEmpty)
        clusters += new Cluster(point)
      else {
        for (another <- nearby.tail) {
          nearby.head ++= another
          clusters -= another
        }
        nearby.head += point
      }
    }

    val locations = for (cl <- clusters.toArray) yield {
      val cluster = cl.toArray
      val latitude = cluster.map(_.latitude).sum / cluster.length
      val longitude = cluster.map(_.longitude).sum / cluster.length
      new LocationExt(latitude, longitude, cluster)
    }
    val convertedTrace = rawTrace.map(x => x.asInstanceOf[SpaceTime])

    var prev = convertedTrace(0)
    val prev_loc = prev.location.asInstanceOf[LocationExt]
    prev_loc.add(maxTimeInterval)

    for (point <- convertedTrace.tail) {
      val prev_loc = prev.location.asInstanceOf[LocationExt]
      val loc = point.location.asInstanceOf[LocationExt]

      val timeInterval = point.timestamp - prev.timestamp

      if (loc == prev_loc) {
        loc.add(min(timeInterval, maxTimeInterval))
      }
      else {
        if (timeInterval < maxTimeInterval) {
          prev_loc.add((maxTimeInterval - timeInterval) / 2)
          loc.add((maxTimeInterval + timeInterval) / 2)
        }
        else {
          loc.add(maxTimeInterval)
        }
      }
      prev = point
    }

    quickSort(locations, (x: LocationExt, y: LocationExt) => x.timeSpan > y.timeSpan)
    Tuple2(convertedTrace, locations)
  }
}
