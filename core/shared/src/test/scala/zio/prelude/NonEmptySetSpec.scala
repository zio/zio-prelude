package zio.prelude

import zio.test._
import zio.test.laws._
import zio.{Has, Random}

object NonEmptySetSpec extends DefaultRunnableSpec {

  private lazy val genSet: Gen[Has[Random] with Has[Sized], Set[Int]] =
    Gen.setOf1(genInt)

  private lazy val genSetFunction: Gen[Has[Random] with Has[Sized], Int => Set[Int]] =
    Gen.function(genSet)

  private lazy val genInt: Gen[Has[Random], Int] =
    Gen.int(-10, 10)

  private lazy val genNonEmptySet: Gen[Has[Random] with Has[Sized], NonEmptySet[Int]] =
    genSet.map(NonEmptySet.fromSetOption(_).get)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptySetSpec")(
      suite("laws")(
        test("commutativeEither")(checkAllLaws(CommutativeEither)(GenFs.nonEmptySet, Gen.anyInt)),
        test("commutative")(checkAllLaws(Commutative)(genNonEmptySet)),
        test("idempotent")(checkAllLaws(Idempotent)(genNonEmptySet)),
        test("hash")(checkAllLaws(Hash)(genNonEmptySet))
      ),
      suite("methods")(
        test("++") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get ++ NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as ++ bs
            actual <-> expected
          }
        },
        test("--") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get -- NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as -- bs
            actual <-> expected
          }
        },
        test("&") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySet.fromSetOption(as).get & NonEmptySet.fromSetOption(bs).get).toSet
            val expected = as & bs
            actual <-> expected
          }
        },
        test("contains") {
          check(genSet, genInt) { (as, a) =>
            NonEmptySet.fromSetOption(as).get.contains(a) <-> as.contains(a)
          }
        },
        test("+") {
          check(genSet, genInt) { (as, a) =>
            val actual   = (NonEmptySet.fromSetOption(as).get + a).toSet
            val expected = as + a
            actual <-> expected
          }
        },
        test("-") {
          check(genSet, genInt) { (as, a) =>
            val actual   = NonEmptySet.fromSetOption(as).get - a
            val expected = as - a
            actual <-> expected
          }
        },
        test("size") {
          check(genSet) { as =>
            val actual   = NonEmptySet.fromSetOption(as).get.size
            val expected = as.size
            actual <-> expected
          }
        },
        test("flatten") {
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
        test("fromSetOption") {
          check(genSet) { as =>
            val nonEmptySet = NonEmptySet.fromSetOption(as).get
            val set         = nonEmptySet.toSet
            assert(set)(equalTo(as))
          }
        }
      )
    )
}
