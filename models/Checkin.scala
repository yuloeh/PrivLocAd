package models

class Checkin private[models](lat: Double, lon: Double, val timestamp: Int) extends Coordinate(lat, lon) {
  def SpaceTime(location: Location): SpaceTime = {
    val spaceTime = this.asInstanceOf[SpaceTime]
    spaceTime._actual = location
    return spaceTime
  }
}
