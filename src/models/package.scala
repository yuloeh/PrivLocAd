import utils.quickSort

import scala.collection.mutable.ListBuffer

package object models {
  def SpaceTime(lat: Double, lon: Double, timestamp: Int): SpaceTime =
    new Checkin(lat, lon, timestamp).asInstanceOf[SpaceTime]

  def cluster(trace: Array[SpaceTime], distThr: Double): Array[Location] = {
    val clusters = new ListBuffer[ListBuffer[SpaceTime]]

    for (point <- trace) {
      val nearby = clusters.filter((x: ListBuffer[SpaceTime]) => x.exists((y: SpaceTime) => point.distance(y) < distThr))
      if (nearby.isEmpty) {
        clusters += ListBuffer(point)
      }
      else {
        for (tomerge <- nearby.tail) {
          nearby.head ++= tomerge
          clusters -= tomerge
        }
        nearby.head += point
      }
    }

    val locations = for (cluster <- clusters.toArray) yield {
      val latitude = cluster.map(_.latitude).sum / cluster.length
      val longitude = cluster.map(_.longitude).sum / cluster.length
      new Location(latitude, longitude, cluster.toArray)
    }

    quickSort(locations, (x: Location, y: Location) => x.checkins.length > y.checkins.length)
    return locations
  }

}
