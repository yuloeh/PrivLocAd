package models

import utils.quickSort

import scala.collection.mutable.ListBuffer

class LocationProfile(rawTrace: Array[Checkin]) extends Profile {

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
      new Location(latitude, longitude, cluster)
    }
    val convertedTrace = rawTrace.map(x => x.asInstanceOf[SpaceTime])

    quickSort(locations, (x: Location, y: Location) => x.checkins.length > y.checkins.length)
    Tuple2(convertedTrace, locations)
  }
}
