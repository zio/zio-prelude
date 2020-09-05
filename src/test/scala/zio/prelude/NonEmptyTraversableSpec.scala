package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object NonEmptyTraversableSpec extends DefaultRunnableSpec {

  val genInt: Gen[Random, Int] =
    Gen.anyInt

  val genNonEmptyList: Gen[Random with Sized, NonEmptyList[Int]] =
    Gens.nonEmptyListOf(genInt)

  val genIntFunction: Gen[Random, Int => Int] =
    Gen.function(genInt)

  val genIntFunction2: Gen[Random, (Int, Int) => Int] =
    Gen.function2(genInt)

  def spec =
    suite("NonEmptyTraversableSpec")(
      suite("instances")(
        testM("nonEmptyChunk")(checkAllLaws(NonEmptyTraversable)(GenFs.nonEmptyChunk, Gen.anyInt))
      ),
      suite("combinators")(
        testM("max") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyTraversable[NonEmptyList].max(as)
            val expected = as.max
            assert(actual)(equalTo(expected))
          }
        },
        testM("maxBy") {
          check(genNonEmptyList, genIntFunction) { (as, f) =>
            val actual   = NonEmptyTraversable[NonEmptyList].maxBy(as)(f)
            val expected = as.maxBy(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("min") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyTraversable[NonEmptyList].min(as)
            val expected = as.min
            assert(actual)(equalTo(expected))
          }
        },
        testM("minBy") {
          check(genNonEmptyList, genIntFunction) { (as, f) =>
            val actual   = NonEmptyTraversable[NonEmptyList].minBy(as)(f)
            val expected = as.minBy(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("reduce") {
          check(genNonEmptyList, genIntFunction2) { (as, f) =>
            val actual   = NonEmptyTraversable[NonEmptyList].reduce(as)(f)
            val expected = as.reduce(Associative.make(f))
            assert(actual)(equalTo(expected))
          }
        },
        testM("toNonEmptyChunk") {
          check(genNonEmptyList) { (as) =>
            val actual   = NonEmptyTraversable[NonEmptyList].toNonEmptyChunk(as)
            val expected = as.toNonEmptyChunk
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
