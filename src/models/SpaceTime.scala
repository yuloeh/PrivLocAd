package models

class SpaceTime private[models](lat: Double, lon: Double, val timestamp: Int) extends Coordinate(lat, lon) {
  def Checkin(location: Location): Checkin = {
    val checkin = this.asInstanceOf[Checkin]
    checkin._actual = location
    return checkin
  }
}
