package zio.prelude

import zio.prelude.newtypes._
import zio.random.Random
import zio.test._
import zio.test.laws._

object IdempotentSpec extends DefaultRunnableSpec {

  val anyMaxInt: Gen[Random, Max[Int]] = Gen.anyInt.map(Max(_))

  def spec: ZSpec[Environment, Failure] =
    suite("IdempotentSpec")(
      suite("laws")(
        testM("boolean disjunction")(checkAllLaws(Idempotent)(Gen.boolean.map(Or(_)))),
        testM("boolean conjuction")(checkAllLaws(Idempotent)(Gen.boolean.map(And(_)))),
        testM("option")(checkAllLaws(Idempotent)(Gen.option(anyMaxInt))),
        testM("set")(checkAllLaws(Idempotent)(Gen.setOf(Gen.anyInt))),
        testM("map")(checkAllLaws(Idempotent)(Gen.mapOf(anyMaxInt, anyMaxInt))),
        testM("tuple2")(checkAllLaws(Idempotent)(anyMaxInt.zip(anyMaxInt))),
        testM("tuple3")(
          checkAllLaws(Idempotent)(anyMaxInt.zip(anyMaxInt).zip(anyMaxInt).map { case ((x, y), z) => (x, y, z) })
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
