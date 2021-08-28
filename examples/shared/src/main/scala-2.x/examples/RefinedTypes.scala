// scalafix:off
package examples

import zio.prelude.refined._

object RefinedTypes extends App {
  import Assertion._

  type Natural <: Int
  val Natural = Refined[Int, Natural] {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)
  }
  implicit class NaturalOps(private val self: Natural) extends AnyVal {
    def add(that: Natural): Natural = Natural.unsafeApply(Natural.unwrap(self) + Natural.unwrap(that))
  }

  type Age <: Int
  val Age = Refined[Int, Age] {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
  }

  val natural: Natural = Natural(5) add Natural(6)

  val age: Age = Age(60)

  def sum(a: Int, b: Int): Int = a + b

  println(sum(natural, age))

  val x: Natural                 = Natural(0)
  val y: Either[String, Natural] = Natural.make(scala.util.Random.nextInt)

  import Regex._

  type MyRegex <: String
  val MyRegex = Refined[String, MyRegex] {
    matches {
      start ~ anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
        "hello" ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3) ~ end
    }
  }

  val myRegex: MyRegex = MyRegex("ab#l*helloccayj678")
}
// scalafix:on
