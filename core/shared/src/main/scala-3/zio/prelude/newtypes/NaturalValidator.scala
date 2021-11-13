package zio.prelude

object NaturalValidator extends Validator[Int](Assertion.greaterThanOrEqualTo(0))

