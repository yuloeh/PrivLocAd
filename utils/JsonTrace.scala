package utils

import com.alibaba.fastjson.{JSONArray, JSONObject}
import models.Checkin

import scala.collection.mutable.ArrayBuffer

object JsonTrace {
  def defaultTransform(node: JSONObject): Checkin = {
    val lat = node.getDouble("latitude")
    val lon = node.getDouble("longitude")
    val tim = node.getIntValue("timestamp")
    return Checkin(lat, lon, tim)
  }

  def cons(traceJsonArray: JSONArray, filter: JSONObject => Boolean = (x: JSONObject) => true, transform: JSONObject => Checkin = defaultTransform): Array[Checkin] = {
    val traceBuff = new ArrayBuffer[Checkin]
    for (i <- traceJsonArray.size - 1 to 0 by -1) {
      val node = traceJsonArray.getJSONObject(i)
      if (filter(node)) traceBuff += transform(node)
    }
    return traceBuff.toArray
  }
}
