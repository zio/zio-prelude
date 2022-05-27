package examples

import zio.prelude.Validator
import zio.prelude.Assertion._
import zio.prelude.Assertion.Regex._
import scala.language.postfixOps

object NaturalValidator extends Validator[Int](
  greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)
)

object AgeValidator extends Validator[Int](
  greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
)

object MyRegexValidator extends Validator[String](
    matches {
      start ~ anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace.* ~ digit.min(0) ~
        nonDigit.min(1) ~ literal("hello").+ ~ anyCharOf('a', 'b', 'c').min(2) ~ notAnyCharOf('d', 'e', 'f').? ~
        inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3) ~ end
    }
)

object CustomFunctionExampleValidator extends Validator[Int](predicate(_ > 10, "bigger than 10"))
