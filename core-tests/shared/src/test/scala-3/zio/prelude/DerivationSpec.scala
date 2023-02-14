package zio.prelude

import zio._
import zio.test._

object DerivationSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int) derives Equal

  sealed trait Color derives Equal

  object Color {
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color
  }

  def spec =
    suite("DerivationSpec")(
      suite("equal")(
        test("case class") {
          assertTrue(Person("Jane Doe", 42) === Person("Jane Doe", 42)) &&
          assertTrue(Person("Jane Doe", 42) !== Person("John Doe", 42))
        },
        test("sealed trait") {
          assertTrue(Color.Red === Color.Red) &&
          assertTrue(Color.Red !== Color.Green)
        }
      )
    )
}
