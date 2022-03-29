package models

class Location(lat: Double, lon: Double, clusteredCheckins: Array[SpaceTime]) extends Coordinate(lat, lon) {
  val checkins : Array[Checkin] = clusteredCheckins.map(_.Checkin(this))
}
