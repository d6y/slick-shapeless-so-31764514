import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object GetResultsExample extends App {

  import slick.driver.H2Driver.api._
  import shapeless._
  import slickless._

  type User =  String :: Int :: Char :: Float :: Float :: Int :: String :: String :: Boolean :: Boolean :: String :: String ::
    String :: String :: String :: String :: String :: String :: String :: String :: Int :: Boolean :: String :: String  :: Long :: HNil

  final class UserTable(tag: Tag) extends Table[User](tag, "users") {
    def id           = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name         = column[String]("name")
    def age          = column[Int]("age")
    def gender       = column[Char]("gender")
    def height       = column[Float]("height_m")
    def weight       = column[Float]("weight_kg")
    def shoeSize     = column[Int]("shoe_size")
    def email        = column[String]("email_address")
    def phone        = column[String]("phone_number")
    def accepted     = column[Boolean]("terms")
    def sendNews     = column[Boolean]("newsletter")
    def street       = column[String]("street")
    def city         = column[String]("city")
    def country      = column[String]("country")
    def faveColor    = column[String]("fave_color")
    def faveFood     = column[String]("fave_food")
    def faveDrink    = column[String]("fave_drink")
    def faveTvShow   = column[String]("fave_show")
    def faveMovie    = column[String]("fave_movie")
    def faveSong     = column[String]("fave_song")
    def lastPurchase = column[String]("sku")
    def lastRating   = column[Int]("service_rating")
    def tellFriends  = column[Boolean]("recommend")
    def petName      = column[String]("pet")
    def partnerName  = column[String]("partner")

    def * = name :: age :: gender :: height :: weight :: shoeSize ::
            email :: phone :: accepted :: sendNews ::
            street :: city :: country ::
            faveColor :: faveFood :: faveDrink :: faveTvShow :: faveMovie :: faveSong ::
            lastPurchase :: lastRating :: tellFriends ::
            petName :: partnerName :: id :: HNil
  }

    lazy val users = TableQuery[UserTable]

    import slick.jdbc.{ GetResult, PositionedResult }
    implicit val charGetResult = GetResult(r => r.nextString.head)

    // GetResult for a HList created by shapeless:

    implicit object hnilGetResult extends GetResult[HNil] {
      def apply(r: PositionedResult) = HNil
    }

    implicit def hlistConsGetResult[H, T <: HList]
      (implicit
        h: GetResult[H],
        t: GetResult[T]
      ) =
        new GetResult[H :: T] {
          def apply(r: PositionedResult) = r.<<[H] :: t(r)
        }


    // Create and schema and populate the DB:
    val setup = DBIO.seq(
      (users.schema).create,
      users +=
        "Dr. Dave Bowman" :: 43 :: 'M' :: 1.7f :: 74.2f :: 11 :: "dave@example.org" :: "+1555740122" :: true :: true ::
        "123 Some Street" :: "Any Town" :: "USA" ::
        "Black" :: "Ice Cream" :: "Coffee" :: "Sky at Night" :: "Silent Running" :: "Bicycle made for Two" ::
        "Acme Space Helmet" :: 10 :: true ::
        "HAL" :: "Betty" :: 0L :: HNil)
    val db = Database.forConfig("h2")
    Await.result(db.run(setup), 2 seconds)

  // Query to see what's in the database:
  val result = Await.result(db.run(users.result), 2 seconds)
  println(s"Result of action is: $result")

  // Going to a case class from an HList:

  case class UserCaseClass(
    name: String, age: Int, gender: Char, height: Float, weight: Float, shoeSize: Int,
    email: String, phone: String, accepted: Boolean, sendNews: Boolean,
    street: String, city: String, country: String,
    faveColor: String, faveFood: String, faveDrink: String, faveTvShow: String, faveMovie: String, faveSong: String,
    lastPurchase: String, lastRating: Int, tellFriends: Boolean,
    petName: String, partnerName: String, id: Long)

  val gen = Generic[UserCaseClass]
  val caseClasses = result.map(gen.from)
  println(s"As a case class: $caseClasses")

  // Plain SQL mapping of an HList

  // Rememner:
  // - Don't use SELECT *
  // - `User` is a type alias for a very long HList
  val plainSql = sql""" select * from "users" """.as[User]

  val plainSqlResults = Await.result(db.run(plainSql), 2 seconds)
  println(s"Plain SQL GenResult: $plainSqlResults")


  // Additional tests:

  val intNil = implicitly[GetResult[Int :: HNil]]
  val intHlistIntNil = implicitly[GetResult[Int :: (Float :: Int :: HNil) :: Int :: HNil]]

  db.close
}
