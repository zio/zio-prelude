package examples

import zio.prelude.refined.*

object RefinedTypes extends App {
  import Assertion.*

  type Natural <: Int

  // MUST be defined as transparent!
  // https://github.com/lampepfl/dotty/issues/12368
  transparent inline def Natural = Refined[Int, Natural] {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)
  }

  type Age <: Int
  transparent inline def Age = Refined[Int, Age] {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
  }

  val natural: Natural = Natural(5)

  val age: Age = Age(60)

  def add(a: Int, b: Int): Int = a + b

  println(add(natural, age))

  val x: Natural                 = Natural(0)
  val y: Either[String, Natural] = Natural(scala.util.Random.nextInt)

  import Regex.*

  type MyRegex <: String
  transparent inline def MyRegex = Refined[String, MyRegex] {
    matches {
      start ~ anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
        "hello" ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3) ~ end
    }
  }

  val myRegex: MyRegex = MyRegex("ab#l*helloccayj678")
}
