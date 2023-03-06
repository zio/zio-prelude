package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes._
import zio.test._
import zio.test.laws._

import scala.math.abs

object IdempotentSpec extends ZIOBaseSpec {

  val anyMaxInt: Gen[Any, Max[Int]] = Gen.int.map(Max(_))

  val anyOrdering: Gen[Any, Ordering] = Gen.int.map { n =>
    abs(n) % 3 match {
      case 0 => Ordering.LessThan
      case 1 => Ordering.Equals
      case 2 => Ordering.GreaterThan
    }
  }

  val anyPartialOrdering: Gen[Any, PartialOrdering] = Gen.int.map { n =>
    abs(n) % 4 match {
      case 0 => Ordering.LessThan
      case 1 => Ordering.Equals
      case 2 => Ordering.GreaterThan
      case 3 => PartialOrdering.Incomparable
    }
  }

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: Spec[Environment, Any] =
    suite("IdempotentSpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(IdempotentLaws)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(IdempotentLaws)(Gen.boolean.map(Or(_)))),
        test("double max")(checkAllLaws(IdempotentLaws)(Gen.double.map(Max(_)))),
        test("double min")(checkAllLaws(IdempotentLaws)(Gen.double.map(Min(_)))),
        test("float max")(checkAllLaws(IdempotentLaws)(Gen.float.map(Max(_)))),
        test("float min")(checkAllLaws(IdempotentLaws)(Gen.float.map(Min(_)))),
        test("map")(checkAllLaws(IdempotentLaws)(Gen.mapOf(anyMaxInt, anyMaxInt))),
        test("option")(checkAllLaws(IdempotentLaws)(Gen.option(anyMaxInt))),
        test("ordering")(checkAllLaws(IdempotentLaws)(anyOrdering)),
        test("partial ordering")(checkAllLaws(IdempotentLaws)(anyPartialOrdering)),
        test("set")(checkAllLaws(IdempotentLaws)(Gen.setOf(Gen.int).map(OrF(_)))),
        test("tuple2")(checkAllLaws(IdempotentLaws)(anyMaxInt.zip(anyMaxInt))),
        test("tuple3")(
          checkAllLaws(IdempotentLaws)(anyMaxInt.zip(anyMaxInt).zip(anyMaxInt))
        )
      ),
      test("Idempotent.reduceIdempotent") {

        assert(List[Max[Int]]().reduceIdempotent)(equalTo(None)) &&
        assert(List(Max(1)).reduceIdempotent)(equalTo(Some(Max(1)))) &&
        assert(List(Max(1), Max(1)).reduceIdempotent)(equalTo(Some(Max(1)))) &&
        assert(List(Max(1), Max(2), Max(1)).reduceIdempotent)(equalTo(Some(Max(2)))) &&
        assert(List(Max(1), Max(1), Max(2), Max(3), Max(3), Max(3), Max(1)).reduceIdempotent)(
          equalTo(Some(Max(3)))
        )
      }
    )
}
