import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Example extends App {

  import slick.driver.H2Driver.api._

  // --

  import shapeless._
  import syntax.std.tuple._

  import scala.reflect.ClassTag
  import slick.lifted.{ProvenShape, MappedScalaProductShape}

/*
abstract class MappedProductShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends ProductNodeShape[Level, C, M, U, P] {
  override def toNode(value: Mixed) = TypeMapping(super.toNode(value), MappedScalaType.Mapper(toBase, toMapped, None), classTag)
  def toBase(v: Any) = new ProductWrapper(getIterator(v.asInstanceOf[C]).toIndexedSeq)
  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))
  def classTag: ClassTag[U]
}

override def getIterator(value: C) = value.productIterator
  def getElement(value: C, idx: Int) = value.productElement(idx)
}
*/
  final class HListShape[
    Level <: ShapeLevel, M <: HList, U <: HList : ClassTag, P <: HList]
    (val shapes: Seq[Shape[_, _, _, _]]) extends MappedProductShape[Level, HList, M, U, P] {
     def buildValue(elems: IndexedSeq[Any]) = elems.foldRight(HNil: HList)(_ :: _)
     def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new HListShape(shapes)

    def classTag: ClassTag[U] = implicitly

    def runtimeList(l: HList): List[Any] = {
        def loop(l: HList, acc: List[Any]): List[Any] = l match {
          case HNil => acc
          case hd :: tl => loop(tl, hd :: acc)
        }
        loop(l, Nil).reverse
      }

    override def getIterator(value: HList) = runtimeList(value).iterator
    def getElement(value: HList, idx: Int) = runtimeList(value)(idx)
   }

   implicit def hnilShape[Level <: ShapeLevel]: HListShape[Level, HNil, HNil, HNil] =
     new HListShape[Level, HNil, HNil, HNil](Nil)

   implicit def hconsShape[Level <: ShapeLevel, M1, M2 <: HList, U1, U2 <: HList, P1, P2 <: HList]
     (implicit s1: Shape[_ <: Level, M1, U1, P1], s2: HListShape[_ <: Level, M2, U2, P2]):
      HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2] =
        new HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2](s1 +: s2.shapes)



  case class User(id: Long, email: String)

  val userGen = Generic[User]

  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id    = column[Long]( "id", O.PrimaryKey, O.AutoInc )
    def email = column[String]("email")

    def * = (id :: email :: HNil) <> ((hlist: Long :: String :: HNil) => userGen.from(hlist), (user: User) => Some(userGen.to(user)))
  }

  lazy val users = TableQuery[Users]

  // Create and schema and populate the DB:
  val setup = DBIO.seq(
    (users.schema).create,
    users += User(1L, "bob@example.org")
  )
  val db = Database.forConfig("h2")
  Await.result(db.run(setup), 2 seconds)

  // Query to see what's in the database:
  val result = Await.result(db.run(users.result), 2 seconds)
  println(s"Result of action is: $result")

  db.close
}