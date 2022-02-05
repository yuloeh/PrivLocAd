package models

class LocationExt(lat: Double, lon: Double, clusteredCheckins: Array[Checkin], p_timeSpan: Int) extends Location(lat, lon, clusteredCheckins) {
  private var _timeSpan: Int = p_timeSpan

  private[models] def this(lat: Double, lon: Double, clusteredCheckins: Array[Checkin]) {
    this(lat, lon, clusteredCheckins, 0)
  }

  private[models] def add(timeSpan: Int): Unit = { _timeSpan += timeSpan }

  def timeSpan: Int = _timeSpan // unit: seconds
}
