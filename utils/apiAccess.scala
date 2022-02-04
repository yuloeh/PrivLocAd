package utils

import com.alibaba.fastjson.{JSON, JSONArray}
import com.tagphi.common.utils.HttpRequestSender

object apiAccess {
  val addr = "https://api.rtbasia.com/coor/get_device_trace"
  val key = "YULE201709251736930423"

  def traceJSON(devID: String): String = HttpRequestSender.sendRequest(addr + "?key=" + key + "&did=" + devID)

  def getTrace(devID: String): JSONArray = {
    val traceJOb = JSON.parseObject(traceJSON(devID))
    return traceJOb.getJSONArray("traces")
  }
}
