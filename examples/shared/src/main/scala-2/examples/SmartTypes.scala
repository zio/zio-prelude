// scalafix:off
package examples

import zio.prelude._

object SmartTypes extends App {
  import Assertion._

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override def assertion = assert(greaterThanOrEqualTo(0) && lessThanOrEqualTo(100))

    implicit class NaturalOps(private val self: Natural) extends AnyVal {
      def add(that: Natural): Natural =
        wrap(unwrap(self) + unwrap(that))
    }
  }

  type Age = Age.Type
  object Age extends Subtype[Int] {
    override def assertion = assert {
      greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
    }
  }

  val natural: Natural = Natural(5) add Natural(6)

  val age: Age = Age(60)

  def sum(a: Int, b: Int): Int = a + b

  println(sum(natural, age))

  val x: Natural                     = Natural(0)
  val y: Validation[String, Natural] = Natural.make(scala.util.Random.nextInt())

  import Regex._

  type MyRegex = MyRegex.Type
  object MyRegex extends Newtype[String] {
    override def assertion = assert {
      matches {
        anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace.* ~ digit.min(0) ~ nonDigit.min(1) ~
          literal("hello").+ ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
          inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
      }
    }
  }

  val myRegex: MyRegex = MyRegex("ab#l*helloccayj678")
}
// scalafix:on
