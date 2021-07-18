package examples

import zio.prelude.refined.Assertion.*
import zio.prelude.refined.macros.*

object Test extends App {
  import Regex.*

  type Natural <: Int

  // MUST be defined as transparent!
  // https://github.com/lampepfl/dotty/issues/12368
  transparent inline def Natural: Int Refined Natural = Refined {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)
  }

  type Age <: Int
  transparent inline def Age: Int Refined Age = Refined {
    greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
  }

  val natural: Natural = Natural(5)

  val age: Age = Age(60)

  def add(a: Int, b: Int): Int = a + b

  println(add(natural, age))

  val x: Natural                 = Natural(0)
  val y: Either[String, Natural] = Natural(scala.util.Random.nextInt)

  type RegexRefined <: String
  transparent inline def RegexRefined: String Refined RegexRefined = Refined {
    matches {
      anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
        "hello" ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
    }
  }

  //val myRegex: RegexRefined = RegexRefined("a")
}