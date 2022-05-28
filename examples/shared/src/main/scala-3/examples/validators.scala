package examples

import zio.prelude.Validator
import zio.prelude.Assertion._
import scala.quoted.FromExpr._
import scala.quoted.Type._
import scala.quoted.FromExpr

object ListWithSumSmallerThan10Validator extends Validator[List[Int]](
  predicate(_.sum < 10, "sum of elements is smaller than 10")
)
