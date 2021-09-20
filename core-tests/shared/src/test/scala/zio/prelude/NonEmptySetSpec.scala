package zio.prelude

import zio.prelude.laws._
import zio.random.Random
import zio.test._
import zio.test.laws._

object NonEmptySetSpec extends DefaultRunnableSpec {

  private lazy val genSet: Gen[Random with Sized, Set[Int]] =
    Gen.setOf1(genInt)

  private lazy val genSetFunction: Gen[Random with Sized, Int => Set[Int]] =
    Gen.function(genSet)

  private lazy val genInt: Gen[Random, Int] =
    Gen.int(-10, 10)

  private lazy val genNonEmptySet: Gen[Random with Sized, NonEmptySet[Int]] =
    genSet.map(NonEmptySet.fromSetOption(_).get)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptySetSpec")(
      suite("laws")(
        testM("commutativeEither")(checkAllLaws(CommutativeEitherLaws)(GenFs.nonEmptySet, Gen.anyInt)),
        testM("commutative")(checkAllLaws(CommutativeLaws)(genNonEmptySet)),
        testM("idempotent")(checkAllLaws(IdempotentLaws)(genNonEmptySet)),
        testM("hash")(checkAllLaws(HashLaws)(genNonEmptySet))
      ),
      suite("methods")(
        testM("++") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get ++ NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as ++ bs
            actual <-> expected
          }
        },
        testM("--") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get -- NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as -- bs
            actual <-> expected
          }
        },
        testM("&") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get & NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as & bs
            actual <-> expected
          }
        },
        testM("contains") {
          check(genSet, genInt) { (as, a) =>
            NonEmptySet.fromSetOption(as).get.contains(a) <-> as.contains(a)
          }
        },
        testM("+") {
          check(genSet, genInt) { (as, a) =>
            val actual   = (NonEmptySet.fromSetOption(as).get + a).toSet
            val expected = as + a
            actual <-> expected
          }
        },
        testM("-") {
          check(genSet, genInt) { (as, a) =>
            val actual   = NonEmptySet.fromSetOption(as).get - a
            val expected = as - a
            actual <-> expected
          }
        },
        testM("size") {
          check(genSet) { as =>
            val actual   = NonEmptySet.fromSetOption(as).get.size
            val expected = as.size
            actual <-> expected
          }
        },
        testM("flatten") {
          check(genSet, genSetFunction) { (as, f) =>
            val actual   = NonEmptySet
              .fromIterableOption(as.toList.map(a => NonEmptySet.fromSetOption(f(a)).get))
              .get
              .flatten
              .toSet
            val expected = as.map(f).flatten
            actual <-> expected
          }
        }
      ),
      suite("constructors")(
        testM("fromSetOption") {
          check(genSet) { as =>
            val nonEmptySet = NonEmptySet.fromSetOption(as).get
            val set         = nonEmptySet.toSet
            assert(set)(equalTo(as))
          }
        }
      )
    )
}
