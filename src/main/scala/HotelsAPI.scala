import scala.compat.Platform
import scala.util.{Success, Failure, Try}

/**
 * Created by mantas on 15.2.4.
 */

case class RoomType(name: String,
                    noOfRooms: Int,
                    timeCreated: Option[Long] = None,
                    amenities: Option[Seq[String]] = None)

case class Hotel(title: String,
                 roomTypes: Seq[RoomType] = Nil)

case class HotelDirectory(hotels: Seq[Hotel] = Nil)



class HotelsAPI {
  val californiaHotel = Hotel("California", roomTypes = Seq(
    RoomType("Deluxe", 25, Some(currentTime), amenities = Some(Seq("xyz", "def"))),
    RoomType("Suite", 10, Some(currentTime))
  ))

  val sandyBeachHotel = Hotel("Sandy Beach Hotel", roomTypes = Seq(
    RoomType("Basic", 5, Some(currentTime)),
    RoomType("Superior", 10, Some(currentTime))
  ))

  val allHotels = Seq(californiaHotel, sandyBeachHotel)

  def currentTime = Platform.currentTime

  def getRoomType(name: String, noOfRooms: Int): RoomType =
    name match {
      case "-" => throw new IllegalArgumentException(s"Invalid room type name on $currentTime")
      case name => RoomType(name, noOfRooms, Some(currentTime))
    }

  def getHotel(name: String): Try[Hotel] =
    name match {
      case "-" => Failure(new IllegalStateException(s"Invalid hotel name on $currentTime"))
      case "California" =>
        Success(californiaHotel)
      case name =>
        Success(sandyBeachHotel)
    }

  def getDirectory: Try[HotelDirectory] = Success {
    HotelDirectory(allHotels)
  }

}
