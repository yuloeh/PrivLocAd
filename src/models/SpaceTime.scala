package models

class SpaceTime(lat: Double, lon: Double, timestamp: Int, p_location: Location) extends Checkin(lat, lon, timestamp) {
  private[models] var _actual: Location = p_location

  // allow partial initiation
  private[models] def this(lat: Double, lon: Double, timestamp: Int) {
    this(lat, lon, timestamp, null)
  }

  def location: Location = _actual
}
