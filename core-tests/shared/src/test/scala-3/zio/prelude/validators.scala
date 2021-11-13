package zio.prelude

import zio.prelude.Assertion._
import zio.prelude.Assertion.Regex._

object NaturalValidator extends Validator[Int](greaterThanOrEqualTo(0))

object LuckyNumberValidator extends Validator[Double](between(10.0, 20.0))

// Mega Email Regex pilfered from https://stackoverflow.com/a/201378
object EmailValidator extends Validator[String](
  matches(
    "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])".r
  )
)

object UsernameValidator extends Validator[String](matches("\\w{3,8}\\d\\d"))

object PasswordValidator extends Validator[String](matches(literal("a").+ ~ anyChar.between(1, 3) ~ literal("oh").*))

object PowerOfTwoValidator extends Validator[Int](powerOf(2))

object PinValidator extends Validator[Int](between(1000, 9999))
