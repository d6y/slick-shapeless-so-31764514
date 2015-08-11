import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Explore extends App {

  import slick.driver.H2Driver.api._
  import shapeless._
  import slickless._



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

  // GetResults for a Case Class that has an Generic in scope:
  implicit def caseClassGetResult[T, R]
    (implicit
      gen:       Generic.Aux[T, R],
      getResult: GetResult[R]
    ): GetResult[T] =
      new GetResult[T] {
        def apply(r: PositionedResult) = gen.from(getResult(r))
      }

  case class SimpleInt(x: Int)
  implicit val genSimpleInt = Generic[SimpleInt]

  val intNil    = implicitly[GetResult[Int :: HNil]]
  val simpleInt = implicitly[GetResult[SimpleInt]]


  case class SimpleFloat(x: Float)
  implicit val genSimpleFloat = Generic[SimpleFloat]

//  val simpleInt2 = implicitly[GetResult[SimpleInt]]


  //val intSimpleNil = implicitly[GetResult[Int:: Simple :: HNil]]
  //val simpleHlistIntNil = implicitly[GetResult[Simple :: (Simple :: Int :: HNil) :: Int :: HNil]]

}
