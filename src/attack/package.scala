import distributions.LaplaceDistribution
import models.{SpaceTime, Coordinate, Location, cluster}

package object attack {
  def obfuscate(point: SpaceTime, scale: Double): SpaceTime = {
    val (x, y) = LaplaceDistribution.sample(scale)
    val obfuscated = privacy.obfuscate(point, x, y)
    return SpaceTime(obfuscated.latitude, obfuscated.longitude, point.timestamp)
  }

  def deobfuscate(obfuscated: Array[SpaceTime], distance: Double): Location ={
    val locations = cluster(obfuscated, 0.35368421 * distance)
    val location = locations(0)
    val obfTop = new Coordinate(location.latitude, location.longitude)
    var center_lat = location.latitude
    var center_lon = location.longitude
    var checkins = location.checkins.map(_.asInstanceOf[SpaceTime])
    var flag = true
    while (flag) {
      val toDrop = checkins.filter(x => x.distance(center_lat, center_lon) > distance)
      if (toDrop.nonEmpty) {
        checkins = checkins.filter(x => obfTop.distance(x) <= distance)
        center_lat = checkins.map((x: SpaceTime) => x.latitude).sum / checkins.length
        center_lon = checkins.map((x: SpaceTime) => x.longitude).sum / checkins.length
        var test = true
        while (test) {
          val neighbor = obfuscated.filter(p => obfTop.distance(p) <= distance)
          if (neighbor.toSet.diff(checkins.toSet).nonEmpty) {
            checkins = neighbor
            center_lat = checkins.map((x: SpaceTime) => x.latitude).sum / checkins.length
            center_lon = checkins.map((x: SpaceTime) => x.longitude).sum / checkins.length
          }
          else
            test = false
        }
      }
      else
        flag = false
    }
    return new Location(center_lat, center_lon, checkins)
  }
}
