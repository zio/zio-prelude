package zio.prelude

import zio.prelude.coherent._
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
      test("Idempotent.optimize") {
        val instance = implicitly[EqualIdempotent[Max[Int]]]

        assert(instance.optimize(List[Max[Int]]()))(equalTo(List[Max[Int]]())) &&
        assert(instance.optimize(List(Max(1))))(equalTo(List(Max(1)))) &&
        assert(instance.optimize(List(Max(1), Max(1))))(equalTo(List(Max(1)))) &&
        assert(instance.optimize(List(Max(1), Max(2), Max(1))))(equalTo(List(Max(1), Max(2), Max(1)))) &&
        assert(instance.optimize(List(Max(1), Max(1), Max(2), Max(3), Max(3), Max(3), Max(1))))(
          equalTo(List(Max(1), Max(2), Max(3), Max(1)))
        )
      },
      testM("Idempotent.optimize's size") {
        val instance = implicitly[EqualIdempotent[Max[Int]]]
        check(Gen.listOf(anyMaxInt)) { list =>
          val actual = instance.optimize(list).size

          assert(actual)(isLessThanEqualTo(list.size)) &&
          assert(actual)(isGreaterThanEqualTo(list.toSet.size))
        }
      }
    )
}
