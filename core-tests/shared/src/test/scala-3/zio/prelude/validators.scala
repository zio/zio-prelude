package zio.prelude

import zio.prelude.Assertion._

object MatchesCustomFunctionValidator extends Validator[Int](
  predicate(_ > 0, "greater than zero")
)

