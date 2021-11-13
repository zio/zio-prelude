package examples

import zio.prelude.Validator
import zio.prelude.Assertion._
import zio.prelude.Assertion.Regex._

object NaturalValidator extends Validator[Int](
  greaterThanOrEqualTo(0) && lessThanOrEqualTo(100)
)

object AgeValidator extends Validator[Int](
  greaterThanOrEqualTo(0) && lessThanOrEqualTo(150)
)

object MyRegexValidator extends Validator[String](
  matches {
    anyChar ~ alphanumeric ~ (nonAlphanumeric | whitespace) ~ nonWhitespace ~ digit.min(0) ~ nonDigit.min(1) ~
      literal("hello") ~ anyOf('a', 'b', 'c').min(2) ~ notAnyOf('d', 'e', 'f').min(0).max(1) ~
      inRange('a', 'z').max(2) ~ notInRange('1', '5').min(1).max(3)
  }
)
