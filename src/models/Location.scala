package models

class Location(lat: Double, lon: Double, clusteredCheckins: Array[Checkin]) extends Coordinate(lat, lon) {
  val checkins : Array[SpaceTime] = clusteredCheckins.map(_.SpaceTime(this))
}
