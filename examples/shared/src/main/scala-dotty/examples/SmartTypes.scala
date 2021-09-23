package examples

import zio.prelude.*

object SmartTypes extends App {
  import Assertion.*

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override inline def assertion = greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)

    extension (self: Natural) {
      infix def add(that: Natural): Natural = wrap(unwrap(self) + unwrap(that))
    }
  }

  type Age = Age.Type
  object Age extends Subtype[Int] {
    override inline def assertion = {
      greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
    }
  }

  val natural: Natural = Natural(5) add Natural(8)

  val age: Age = Age(60)

  def sum(a: Int, b: Int): Int = a + b

  println(sum(natural, age))

  val x: Natural                 = Natural(0)
  // val y: Either[String, Natural] = Natural.unsafeWrap(scala.util.Random.nextInt)

  import Regex.*

  type MyRegex = MyRegex.Type
  object MyRegex extends Newtype[String] {
    override inline def assertion = {
      matches {
        anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
          literal("hello") ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
          inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
      }
    }
  }

  val myRegex: MyRegex = MyRegex("ab#l*helloccayj678")
}
