package examples

import zio.prelude.macros.Refined
import zio.prelude.refined.Assertion._

import Regex._

object Example extends App {
  private val Between10And20 = Refined[Int]((greaterThan(10) && lessThan(20)) || equalTo(30))
  private object Amount extends Between10And20.Newtype {
    type Amount = Amount.Type
    implicit final class AmountOps(private val self: Amount) extends AnyVal {
      def ten = 10
    }
  }

  import Amount.Amount

  val amt: Amount = Amount(30)

  private val JorgeRefined = Refined[String](matches(start ~ literal("Jorge") ~ end))
  private object Jorge extends JorgeRefined.Newtype {
    type Jorge = Jorge.Type
  }
  import Jorge._
  val jorge: Jorge = Jorge("Jorge")

  private val RegexRefined = Refined[String] {
    matches {
      anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
        "hello" ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
    }
  }
  private object MyRegex extends RegexRefined.Newtype {
    type MyRegex = MyRegex.Type
  }
}
