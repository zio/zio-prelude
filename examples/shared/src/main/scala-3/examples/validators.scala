package examples

import zio.prelude.Validator
import zio.prelude.Assertion._
import scala.quoted.FromExpr._
import scala.quoted.Type._
import scala.quoted.FromExpr

object MatchesCustomFunctionValidator extends Validator[Int](
  predicate(_ > 0, "greater than zero")
)
