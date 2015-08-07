import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Example extends App {

  import slick.driver.H2Driver.api._
  import shapeless._
  import syntax.std.tuple._

  import scala.reflect.ClassTag
  import slick.lifted.MappedScalaProductShape
  import io.underscore.slick._

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

  db.close
}
