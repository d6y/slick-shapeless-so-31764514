import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Example extends App {

  import slick.driver.H2Driver.api._

  import shapeless._
  import slickless._

  class Users(tag: Tag) extends Table[Long :: String :: HNil](tag, "users") {
    def id    = column[Long]( "id", O.PrimaryKey, O.AutoInc )
    def email = column[String]("email")

    def * = (id :: email :: HNil)
  }

  lazy val users = TableQuery[Users]

  // Create and schema and populate the DB:
  val setup = DBIO.seq(
    (users.schema).create,
    users += 1L :: "bob@example.org" :: HNil
  )
  val db = Database.forConfig("h2")
  Await.result(db.run(setup), 2 seconds)

  // Query to see what's in the database:
  val result = Await.result(db.run(users.result), 2 seconds)
  println(s"Result of action is: $result")

  // Going to a case class from an HList:
  case class SomeCaseClass(id: Long, email: String)
  val gen = Generic[SomeCaseClass]
  val caseClasses = result.map(gen.from)
  println(s"As a case class: $caseClasses")

  // GetResult type class
  import slick.jdbc.{ GetResult, PositionedResult }

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

  val plainSql =
    sql""" select "id", "email" from "users" """.as[Long :: String :: HNil]

  val plainSqlResults = Await.result(db.run(plainSql), 2 seconds)
  println(s"Plain SQL GenResult: $plainSqlResults")


  // Now for case classes
  case class Contact(id: Long, Email: String)
  implicit val genContact = Generic[Contact]

  implicit def caseClassGetResult[T,R]
    (implicit
      gen:       Generic.Aux[T, R],
      getResult: GetResult[R]
    ): GetResult[T] =
      new GetResult[T] {
        def apply(r: PositionedResult) = gen.from(getResult(r))
      }

  val plainSqlCaseClass =
    sql""" select "id", "email" from "users" """.as[Contact]

  val ccResults = Await.result(db.run(plainSqlCaseClass), 2 seconds)
  println(s"Plain SQL GenResult for CaseClass: $ccResults")


  db.close
}
