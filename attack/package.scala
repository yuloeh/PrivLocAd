import models.{Checkin, Coordinate, Location, cluster}
import org.apache.commons.math3.distribution.{GammaDistribution, UniformRealDistribution}

import scala.collection.mutable.ListBuffer
import scala.math.{Pi, cos, sin}

package object attack {
  def obfuscate(point: Checkin, scale: Double): Checkin = {
    val gamma = new GammaDistribution(2, scale)
    val random_radius = gamma.sample()
    val uniform = new UniformRealDistribution(0, 2)
    val random_angle = uniform.sample() * Pi
    val x = random_radius * cos(random_angle)
    val y = random_radius * sin(random_angle)
    val obfuscated = privacy.obfuscate(point, x, y)
    return Checkin(obfuscated.latitude, obfuscated.longitude, point.timestamp)
  }

  def deobfuscate(obfuscated: Array[Checkin], distance: Double): Location ={
    val locations = cluster(obfuscated, 0.35368421 * distance)
    val location = locations(0)
    val obfTop = new Coordinate(location.latitude, location.longitude)
    var center_lat = location.latitude
    var center_lon = location.longitude
    var checkins = location.checkins.map(_.asInstanceOf[Checkin])
    var flag = true
    while (flag) {
      val toDrop = checkins.filter(x => x.distance(center_lat, center_lon) > distance)
      if (toDrop.nonEmpty) {
        checkins = checkins.filter(x => obfTop.distance(x) <= distance)
        center_lat = checkins.map((x: Checkin) => x.latitude).sum / checkins.length
        center_lon = checkins.map((x: Checkin) => x.longitude).sum / checkins.length
        var test = true
        while (test) {
          val neighbor = obfuscated.filter(p => obfTop.distance(p) <= distance)
          if (neighbor.toSet.diff(checkins.toSet).nonEmpty) {
            checkins = neighbor
            center_lat = checkins.map((x: Checkin) => x.latitude).sum / checkins.length
            center_lon = checkins.map((x: Checkin) => x.longitude).sum / checkins.length
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
