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

    implicit class AnyOps[A](a: A) {
      def some: Option[A] = Some(a)
    }
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
  import slick.jdbc.GetResult
  implicit val getResults: GetResult[SomeCaseClass] =
    GetResult(r => SomeCaseClass(id = r.nextLong, email = r.nextString))

  // NB: in place of nextLong and nextString we can use r.<< for both

  val plainSql =
    sql""" select "id", "email" from "users" """.as[SomeCaseClass]

  val plainSqlResults = Await.result(db.run(plainSql), 2 seconds)
  println(s"Plain SQL GenResult: $plainSqlResults")

  db.close
}
