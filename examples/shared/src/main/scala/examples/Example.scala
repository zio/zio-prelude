package examples

import zio.prelude.macros.Refined
import zio.prelude.refined.Assertion._
import Regex._

object Example extends App {
  val Between10And20 = Refined[Int]((greaterThan(10) && lessThan(20)) || equalTo(30))
  object Amount extends Between10And20.Newtype {
    type Amount = Amount.Type
    implicit final class AmountOps(private val self: Amount) extends AnyVal {
      def ten = 10
    }
  }

  import Amount.Amount

  val amt: Amount = Amount(30)

  val JorgeRefined = Refined[String](matches(start ~ literal("Jorge") ~ end))
  object Jorge extends JorgeRefined.Newtype {
    type Jorge = Jorge.Type
  }
  import Jorge._
  val jorge: Jorge = Jorge("Jorge")

//  val RegexRefined = Refined[String] {
//    matches {
//      any ~ anyChar
//    }
//  }
//  object MyRegex extends RegexRefined.Newtype {
//    type MyRegex = MyRegex.Type
//  }
//
//  import MyRegex._
//  val myRegex: MyRegex = MyRegex("a")

  val RegexRefined = Refined[String] {
    matches {
      anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
        "hello" ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
    }
  }
  object MyRegex extends RegexRefined.Newtype {
    type MyRegex = MyRegex.Type
  }

  import MyRegex._
  val myRegex: MyRegex = MyRegex("a")
}
