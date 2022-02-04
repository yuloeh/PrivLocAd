package map

import models.Coordinate

import scala.math.{acos, atan2, cos, sin, sqrt, toDegrees, toRadians}

class Map(val origin: Coordinate) {

  def wsg84_to_cartesian(coordinate: Coordinate): Cartesian = {
    val alpha = toRadians(coordinate.longitude - origin.longitude)
    val beta = toRadians(coordinate.latitude)
    val costheta = cos(alpha) * cos(beta) / sqrt(1 - sin(alpha) * sin(alpha) * cos(beta) * cos(beta))
    val theta = toDegrees(acos(costheta))
    val h = new Coordinate(theta, origin.longitude)
    val x = if (coordinate.longitude > origin.longitude) coordinate.distance(h) else -coordinate.distance(h)
    val y = toRadians(theta - origin.latitude) * 6378137.0
    return new Cartesian(x, y)
  }

  def cartesian_to_wsg84(x: Double, y: Double): Coordinate = {
    val verticalRadian = y / 6378137.0
    val theta = toRadians(origin.latitude) + verticalRadian
    val radian = x / 6378137.0
    val tanDLon_a = sin(radian)
    val tanDLon_b = cos(theta) * cos(radian)
    val sinDlon = tanDLon_a / sqrt(tanDLon_a * tanDLon_a + tanDLon_b * tanDLon_b)
    val cosLat = sin(radian) / sinDlon
    val lat = toDegrees(acos(cosLat))
    val lon = toDegrees(atan2(tanDLon_a, tanDLon_b)) + origin.longitude

    return new Coordinate(lat, lon)
  }
}
