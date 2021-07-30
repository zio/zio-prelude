package zio.prelude

import zio.prelude.newtypes._
import zio.test._
import zio.test.laws._
import zio.{Has, Random}

import scala.math.abs

object IdempotentSpec extends DefaultRunnableSpec {

  val anyMaxInt: Gen[Has[Random], Max[Int]] = Gen.anyInt.map(Max(_))

  val anyOrdering: Gen[Has[Random], Ordering] = Gen.anyInt.map { n =>
    abs(n) % 3 match {
      case 0 => Ordering.LessThan
      case 1 => Ordering.Equals
      case 2 => Ordering.GreaterThan
    }
  }

  val anyPartialOrdering: Gen[Has[Random], PartialOrdering] = Gen.anyInt.map { n =>
    abs(n) % 4 match {
      case 0 => Ordering.LessThan
      case 1 => Ordering.Equals
      case 2 => Ordering.GreaterThan
      case 3 => PartialOrdering.Incomparable
    }
  }

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("IdempotentSpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(Idempotent)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(Idempotent)(Gen.boolean.map(Or(_)))),
        test("double max")(checkAllLaws(Idempotent)(Gen.anyDouble.map(Max(_)))),
        test("double min")(checkAllLaws(Idempotent)(Gen.anyDouble.map(Min(_)))),
        test("float max")(checkAllLaws(Idempotent)(Gen.anyFloat.map(Max(_)))),
        test("float min")(checkAllLaws(Idempotent)(Gen.anyFloat.map(Min(_)))),
        test("map")(checkAllLaws(Idempotent)(Gen.mapOf(anyMaxInt, anyMaxInt))),
        test("option")(checkAllLaws(Idempotent)(Gen.option(anyMaxInt))),
        test("ordering")(checkAllLaws(Idempotent)(anyOrdering)),
        test("partial ordering")(checkAllLaws(Idempotent)(anyPartialOrdering)),
        test("set")(checkAllLaws(Idempotent)(Gen.setOf(Gen.anyInt))),
        test("tuple2")(checkAllLaws(Idempotent)(anyMaxInt.zip(anyMaxInt))),
        test("tuple3")(
          checkAllLaws(Idempotent)(anyMaxInt.zip(anyMaxInt).zip(anyMaxInt))
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
