import com.alibaba.fastjson.JSONObject
import models.Coordinate

import scala.math.{acos, atan2, cos, sin, sqrt, toDegrees, toRadians}

package object privacy {
  val g_delta_5: Array[Double] = Array(2.9957, 4.7439, 6.2958, 7.7537, 9.1535, 10.513)   // delta = 0.05
  val g_delta_1: Array[Double] = Array(4.6052, 6.6384, 7.406, 10.0451, 11.6046, 13.1085) // delta = 0.01
  //val gridSize = 100
  val MAX_NUM_GEN_LOCATION = 6

  val ln_20 = 2.99573227355
  val ln_100 = 4.605170185988

  def inShanghai(point: JSONObject): Boolean = {
    val latitude = point.getDouble("latitude")
    val longitude = point.getDouble("longitude")
    latitude  > 30.7 && latitude < 31.4 && longitude > 121 && longitude < 122
  }

  def obfuscate(origin: Coordinate, x: Double, y: Double): Coordinate = {
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
