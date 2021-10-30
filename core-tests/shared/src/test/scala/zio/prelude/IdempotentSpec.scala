package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes._
import zio.random.Random
import zio.test._
import zio.test.laws._

import scala.math.abs

object IdempotentSpec extends DefaultRunnableSpec {

  val anyMaxInt: Gen[Random, Max[Int]] = Gen.anyInt.map(Max(_))

  val anyOrdering: Gen[Random, Ordering] = Gen.anyInt.map { n =>
    abs(n) % 3 match {
      case 0 => Ordering.LessThan
      case 1 => Ordering.Equals
      case 2 => Ordering.GreaterThan
    }
  }

  val anyPartialOrdering: Gen[Random, PartialOrdering] = Gen.anyInt.map { n =>
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
        testM("boolean conjuction")(checkAllLaws(IdempotentLaws)(Gen.boolean.map(And(_)))),
        testM("boolean disjunction")(checkAllLaws(IdempotentLaws)(Gen.boolean.map(Or(_)))),
        testM("double max")(checkAllLaws(IdempotentLaws)(Gen.anyDouble.map(Max(_)))),
        testM("double min")(checkAllLaws(IdempotentLaws)(Gen.anyDouble.map(Min(_)))),
        testM("float max")(checkAllLaws(IdempotentLaws)(Gen.anyFloat.map(Max(_)))),
        testM("float min")(checkAllLaws(IdempotentLaws)(Gen.anyFloat.map(Min(_)))),
        testM("map")(checkAllLaws(IdempotentLaws)(Gen.mapOf(anyMaxInt, anyMaxInt))),
        testM("option")(checkAllLaws(IdempotentLaws)(Gen.option(anyMaxInt))),
        testM("ordering")(checkAllLaws(IdempotentLaws)(anyOrdering)),
        testM("partial ordering")(checkAllLaws(IdempotentLaws)(anyPartialOrdering)),
        testM("set")(checkAllLaws(IdempotentLaws)(Gen.setOf(Gen.anyInt).map(OrF(_)))),
        testM("tuple2")(checkAllLaws(IdempotentLaws)(anyMaxInt.zip(anyMaxInt))),
        testM("tuple3")(
          checkAllLaws(IdempotentLaws)(anyMaxInt.zip(anyMaxInt).zip(anyMaxInt).map { case ((x, y), z) => (x, y, z) })
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
