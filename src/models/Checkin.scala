package models

class Checkin(lat: Double, lon: Double, timestamp: Int, centroid: Location) extends SpaceTime(lat, lon, timestamp) {
  // allow partial initiation
  private[models] def this(lat: Double, lon: Double, timestamp: Int) {
    this(lat, lon, timestamp, null)
  }

  private[models] var _actual: Location = centroid
  def location: Location = _actual
}
