import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

/**
 * Created by mantas on 15.2.4.
 */
class HotelsAPISpec extends Specification {

  val api = new HotelsAPI

  "Hotels API" should {
    def haveRoomTypeWithNameAndNoOfRooms(name: String, noOfRooms: Int): Matcher[RoomType] =
      haveName(name) and haveNoOfRooms(noOfRooms)

    def haveName(name: String) =
      be_===(name) ^^ { (_: RoomType).name aka "actual name" }

    def haveNoOfRooms(noOfRooms: Int) =
      be_===(noOfRooms) ^^ { (_: RoomType).noOfRooms aka "no of rooms" }

    "get room type with name and number of rooms" in {
      api.getRoomType("Deluxe", 25) must haveName("Deluxe") and haveNoOfRooms(25)
    }

    "get room type with name and number of rooms" in {
      api.getRoomType("Deluxe", 25) must haveRoomTypeWithNameAndNoOfRooms("Deluxe", 25)
    }

    def haveMessage(messageThatIs: Matcher[String]): Matcher[Throwable] =
      messageThatIs ^^ { (_: Throwable).getMessage aka "exception message" }

    def haveMessageLike(messageLike: String) =
      haveMessage(contain(messageLike))

    def throwIllegalArgument[A](withMessageLike: String): Matcher[RoomType] =
      throwsAnException(beAnInstanceOf[IllegalArgumentException] and haveMessageLike(withMessageLike))

    def throwsAnException(beExceptionThatIs: Matcher[Exception]): Matcher[RoomType] = throwA.like {
      case e: Exception => e must beExceptionThatIs
    }

    def withIllegalState(withMessageLike: String): Matcher[Throwable] =
      beAnInstanceOf[IllegalStateException] and haveMessageLike(withMessageLike)

    "throw IllegalArgumentException with message on invalid input" in {
      api.getRoomType("-", 25) must throwIllegalArgument(withMessageLike = "Invalid room")
    }

    def withNoOfRoomTypes(noOfRoomTypes: Int): Matcher[Hotel] =
      be_===(noOfRoomTypes) ^^ { (_: Hotel).roomTypes.size aka "have no of room types" }

    // TODO
    //def withNoOfRoomTypes(noOfRoomTypes: Int): Matcher[Hotel] =
    //  haveSize[Seq[Int]](noOfRoomTypes) ^^ { (_: Hotel).roomTypes aka "have no of room types" }

    def containRoomTypeWithName(name: String) =
      contain(haveName(name)) ^^ { (_: Hotel).roomTypes aka "contain room type with name" }

    "hotel should contain no of rooms and room with name Deluxe" in {
      api.getHotel("California") must beSuccessfulTry {
        withNoOfRoomTypes(2) and containRoomTypeWithName("Deluxe")
      }
    }

    "throw IllegalStateException with message on invalid hotel input" in {
      api.getHotel("-") must beFailedTry {
        withIllegalState(withMessageLike = "Invalid hotel name")
      }
    }

    def withNoOfHotels(noOfHotels: Int) =
      be_===(noOfHotels) ^^ { (_: HotelDirectory).hotels.size aka "no of hotels" }

    def containHotel(thatIs: Matcher[Hotel]) =
      contain(thatIs) ^^ { (_: HotelDirectory).hotels aka "contain hotel that has room type with name" }

    def withRoomType(thatIs: Matcher[RoomType]) =
      contain(thatIs) ^^ { (_: Hotel).roomTypes aka "with room type" }

    val withName = haveName _

    "directory contains 2 hotels that contain at least one room type with name" in {
      api.getDirectory must beSuccessfulTry {
        withNoOfHotels(2) and containHotel(withRoomType(withName("Deluxe")))
      }
    }

    def withAmenity(name: String) =
      withSomeAmenities(containAllOf(Seq(name)))

    def withSomeAmenities(thatIs: Matcher[Seq[String]]) =
      beSome(thatIs) ^^ { (_: RoomType).amenities aka "has some amenities" }

    "directory contains 2 hotels that contain at least one room type that contains concrete amenity" in {
      api.getDirectory must beSuccessfulTry(containHotel(withRoomType(withAmenity("def"))))
    }
  }
}
