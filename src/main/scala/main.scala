import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Example extends App {

  import slick.driver.H2Driver.api._

  // --

  import shapeless._
  import syntax.std.tuple._

  // --

  class Users(tag: Tag) extends Table[Long :: String :: HNil](tag, "users") {
    def id    = column[Long]( "id", O.PrimaryKey, O.AutoInc )
    def email = column[String]( "email" )

    /** The * projection of the table used as default for queries and inserts.
      * Should include all columns as a tuple, HList or custom shape and optionally
      * map them to a custom entity type using the <> operator.
      * The `ProvenShape` return type ensures that
      * there is a `Shape` available for translating between the `Column`-based
      * type in * and the client-side type without `Column` in the table's type
      * parameter. */
    def * : slick.lifted.ProvenShape[Long :: String :: HNil] =
      (id, email) <>[ Long :: String :: HNil, (Long,String) ] (p => p.productElements, h => Some(h.tupled) )

  }

  lazy val users = TableQuery[Users]

  // Create and schema and populate the DB:
  val setup = DBIO.seq(
    (users.schema).create,
    users += 1 :: "bob@example.org" :: HNil
  )
  val db = Database.forConfig("h2")
  Await.result(db.run(setup), 2 seconds)

  // Query to see what's in the database:
  val result = Await.result(db.run(users.result), 2 seconds)
  println(s"Result of action is: $result")

  db.close
}