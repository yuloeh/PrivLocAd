import utils.quickSort

import scala.collection.mutable.ListBuffer

package object models {
  def Checkin(lat: Double, lon: Double, timestamp: Int): Checkin =
    new SpaceTime(lat, lon, timestamp).asInstanceOf[Checkin]

  def cluster(trace: Array[Checkin], distThr: Double): Array[Location] = {
    val clusters = new ListBuffer[ListBuffer[Checkin]]

    for (point <- trace) {
      val nearby = clusters.filter((x: ListBuffer[Checkin]) => x.exists((y: Checkin) => point.distance(y) < distThr))
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
