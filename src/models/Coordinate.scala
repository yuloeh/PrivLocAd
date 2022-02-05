package models

import map.Cartesian

import scala.math.{acos, atan2, cos, sin, sqrt, toDegrees, toRadians}

class Coordinate (lat: Double, lon: Double) {
  val latitude: Double = lat
  val longitude: Double = lon

  def distance(latitude: Double, longitude: Double): Double = {
    val dLat = toRadians(this.latitude - latitude)
    val dLng = toRadians(this.longitude - longitude)
    val haversine_dLat = sin(dLat / 2) * sin(dLat / 2)
    val haversine_dLng = sin(dLng / 2) * sin(dLng / 2)
    val haversine_theta = haversine_dLat + cos(toRadians(this.latitude)) * cos(toRadians(latitude)) * haversine_dLng
    val theta = 2 * atan2(sqrt(haversine_theta), sqrt(1 - haversine_theta))
    return 6378137.0 * theta
  }

  def distance(coordinate: Coordinate): Double = distance(coordinate.latitude, coordinate.longitude)

  def bd09_to_gcj02: Coordinate = {
    val x = longitude - 0.0065D
    val y = latitude - 0.006D
    val z = Math.sqrt(x * x + y * y) - 2.0E-5D * Math.sin(y * 52.35987755982988D)
    val theta = Math.atan2(y, x) - 3.0E-6D * Math.cos(x * 52.35987755982988D)
    new Coordinate(z * Math.sin(theta), z * Math.cos(theta))
  }

  def gcj02_to_bd09: Coordinate = {
    val x = longitude
    val y = latitude
    val z = Math.sqrt(x * x + y * y) + 2.0E-5D * Math.sin(y * 52.35987755982988D)
    val theta = Math.atan2(y, x) + 3.0E-6D * Math.cos(x * 52.35987755982988D)
    new Coordinate(z * Math.sin(theta) + 0.006D, z * Math.cos(theta) + 0.0065D)
  }

  def wgs84_to_gcj02: Coordinate = {
    val a = 6378245.0D
    val ee = 0.006693421622965943D
    val lat = latitude
    val lon = longitude
    var dLat = transformLat(lon - 105.0D, lat - 35.0D)
    var dLon = transformLon(lon - 105.0D, lat - 35.0D)
    val radLat = lat / 180.0D * 3.141592653589793D
    var magic = Math.sin(radLat)
    magic = 1.0D - ee * magic * magic
    val sqrtMagic = Math.sqrt(magic)
    dLat = dLat * 180.0D / (a * (1.0D - ee) / (magic * sqrtMagic) * 3.141592653589793D)
    dLon = dLon * 180.0D / (a / sqrtMagic * Math.cos(radLat) * 3.141592653589793D)
    new Coordinate(lat + dLat, lon + dLon)
  }

  def gcj02_to_wgs84: Coordinate = {
    val gpt = this.wgs84_to_gcj02
    val lat = gpt.latitude - latitude
    val lon = gpt.longitude - longitude
    new Coordinate(latitude - lat, longitude - lon)
  }

  def wgs84_to_bd09: Coordinate = this.wgs84_to_gcj02.gcj02_to_bd09

  def bd09_to_wgs84: Coordinate = this.bd09_to_gcj02.gcj02_to_wgs84

  private def transformLat(x: Double, y: Double) = {
    var ret = -100.0D + 2.0D * x + 3.0D * y + 0.2D * y * y + 0.1D * x * y + 0.2D * Math.sqrt(Math.abs(x))
    ret += (20.0D * Math.sin(6.0D * x * 3.141592653589793D) + 20.0D * Math.sin(2.0D * x * 3.141592653589793D)) * 2.0D / 3.0D
    ret += (20.0D * Math.sin(y * 3.141592653589793D) + 40.0D * Math.sin(y / 3.0D * 3.141592653589793D)) * 2.0D / 3.0D
    ret += (160.0D * Math.sin(y / 12.0D * 3.141592653589793D) + 320.0D * Math.sin(y * 3.141592653589793D / 30.0D)) * 2.0D / 3.0D
    ret
  }

  private def transformLon(x: Double, y: Double) = {
    var ret = 300.0D + x + 2.0D * y + 0.1D * x * x + 0.1D * x * y + 0.1D * Math.sqrt(Math.abs(x))
    ret += (20.0D * Math.sin(6.0D * x * 3.141592653589793D) + 20.0D * Math.sin(2.0D * x * 3.141592653589793D)) * 2.0D / 3.0D
    ret += (20.0D * Math.sin(x * 3.141592653589793D) + 40.0D * Math.sin(x / 3.0D * 3.141592653589793D)) * 2.0D / 3.0D
    ret += (150.0D * Math.sin(x / 12.0D * 3.141592653589793D) + 300.0D * Math.sin(x / 30.0D * 3.141592653589793D)) * 2.0D / 3.0D
    ret
  }

  def wsg84_to_cartesian(origin: Coordinate): Cartesian = {
    val alpha = toRadians(longitude - origin.longitude)
    val beta = toRadians(latitude)
    val costheta = cos(alpha) * cos(beta) / sqrt(1 - sin(alpha) * sin(alpha) * cos(beta) * cos(beta))
    val theta = toDegrees(acos(costheta))
    val h = new Coordinate(theta, origin.longitude)
    val x = if (longitude > origin.longitude) distance(h) else -distance(h)
    val y = toRadians(theta - origin.latitude) * 6378137.0
    return new Cartesian(x, y)
  }

}
