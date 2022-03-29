package utils

import com.alibaba.fastjson.{JSONArray, JSONObject}
import models.SpaceTime

import scala.collection.mutable.ArrayBuffer

object JsonTrace {
  def defaultTransform(node: JSONObject): SpaceTime = {
    val lat = node.getDouble("latitude")
    val lon = node.getDouble("longitude")
    val tim = node.getIntValue("timestamp")
    return SpaceTime(lat, lon, tim)
  }

  def cons(traceJsonArray: JSONArray, filter: JSONObject => Boolean = (x: JSONObject) => true, transform: JSONObject => SpaceTime = defaultTransform): Array[SpaceTime] = {
    val traceBuff = new ArrayBuffer[SpaceTime]
    for (i <- traceJsonArray.size - 1 to 0 by -1) {
      val node = traceJsonArray.getJSONObject(i)
      if (filter(node)) traceBuff += transform(node)
    }
    return traceBuff.toArray
  }
}
