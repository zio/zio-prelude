package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._
import zio.Random

object NonEmptyForEachSpec extends DefaultRunnableSpec {

  val genInt: Gen[Random, Int] =
    Gen.int

  val genNonEmptyList: Gen[Random with Sized, NonEmptyList[Int]] =
    Gens.nonEmptyListOf(genInt)

  val genIntFunction: Gen[Random, Int => Int] =
    Gen.function(genInt)

  val genIntFunction2: Gen[Random, (Int, Int) => Int] =
    Gen.function2(genInt)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptyForEachSpec")(
      suite("laws")(
        test("nonEmptyChunk")(checkAllLaws(NonEmptyForEachLaws)(GenFs.nonEmptyChunk, Gen.int))
      ),
      suite("combinators")(
        test("max") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyForEach[NonEmptyList].max(as)
            val expected = as.max
            assert(actual)(equalTo(expected))
          }
        },
        test("maxBy") {
          check(genNonEmptyList, genIntFunction) { (as, f) =>
            val actual   = NonEmptyForEach[NonEmptyList].maxBy(as)(f)
            val expected = as.maxBy(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("min") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyForEach[NonEmptyList].min(as)
            val expected = as.min
            assert(actual)(equalTo(expected))
          }
        },
        test("minBy") {
          check(genNonEmptyList, genIntFunction) { (as, f) =>
            val actual   = NonEmptyForEach[NonEmptyList].minBy(as)(f)
            val expected = as.minBy(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("reduceAll") {
          check(genNonEmptyList, genIntFunction2) { (as, f) =>
            val actual   = NonEmptyForEach[NonEmptyList].reduceAll(as)(f)
            val expected = as.reduce(Associative.make(f))
            assert(actual)(equalTo(expected))
          }
        },
        test("toNonEmptyChunk") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyForEach[NonEmptyList].toNonEmptyChunk(as)
            val expected = as.toNonEmptyChunk
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
