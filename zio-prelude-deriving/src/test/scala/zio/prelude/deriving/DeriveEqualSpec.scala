package zio.prelude.deriving

import zio.prelude.deriving.DeriveEqual._
import zio.prelude.Equal
import zio.random.Random
import zio.test._
import zio.test.laws._

object DeriveEqualSpec extends DefaultRunnableSpec {

  final case class Person(name: String, age: Int)

  val genPerson: Gen[Random with Sized, Person] =
    for {
      name <- Gen.anyString
      age  <- Gen.anyInt
    } yield Person(name, age)

  sealed trait Color
  case object Red   extends Color
  case object Green extends Color
  case object Blue  extends Color

  val genColor: Gen[Random, Color] =
    Gen.elements(Red, Green, Blue)

  def spec = suite("DeriveEqualSpec")(
    testM("instances can be derived for case classes") {
      checkAllLaws(Equal)(genPerson)
    },
    testM("instances can be derived for sealed traits") {
      checkAllLaws(Equal)(genColor)
    }
  )
}
