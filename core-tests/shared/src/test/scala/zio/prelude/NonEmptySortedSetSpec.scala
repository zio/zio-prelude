package zio.prelude


import zio.Random
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

import scala.collection.immutable.SortedSet

object NonEmptySortedSetSpec extends DefaultRunnableSpec {

  private lazy val genSet: Gen[Random with Sized, SortedSet[Int]] =
    Gen.setOf1(genInt).map(SortedSet.from(_))

  private lazy val genInt: Gen[Random, Int] =
    Gen.int(-10, 10)

  private lazy val genNonEmptySortedSet: Gen[Random with Sized, NonEmptySortedSet[Int]] =
    genSet.map(NonEmptySortedSet.fromSetOption(_).get)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptySortedSetSpec")(
      suite("laws")(
        test("commutativeEither")(checkAllLaws(CommutativeEitherLaws)(GenFs.nonEmptySet, Gen.int)),
        test("commutative")(checkAllLaws(CommutativeLaws)(genNonEmptySortedSet)),
        test("idempotent")(checkAllLaws(IdempotentLaws)(genNonEmptySortedSet)),
        test("hash")(checkAllLaws(HashLaws)(genNonEmptySortedSet))
      ),
      suite("methods")(
        test("++") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySortedSet.fromSetOption(as).get ++ NonEmptySortedSet.fromSetOption(bs).get).toSet
            val expected = as ++ bs
            actual <-> expected
          }
        },
        test("--") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySortedSet.fromSetOption(as).get -- NonEmptySortedSet.fromSetOption(bs).get).toSet
            val expected = as -- bs
            actual <-> expected
          }
        },
        test("&") {
          check(genSet, genSet) { (as, bs) =>
            val actual   = (NonEmptySortedSet.fromSetOption(as).get & NonEmptySortedSet.fromSetOption(bs).get).toSet
            val expected = as & bs
            actual <-> expected
          }
        },
        test("contains") {
          check(genSet, genInt) { (as, a) =>
            NonEmptySortedSet.fromSetOption(as).get.contains(a) <-> as.contains(a)
          }
        },
        test("+") {
          check(genSet, genInt) { (as, a) =>
            val actual   = (NonEmptySortedSet.fromSetOption(as).get + a).toSet
            val expected = as + a
            actual <-> expected
          }
        },
        test("-") {
          check(genSet, genInt) { (as, a) =>
            val actual   = NonEmptySortedSet.fromSetOption(as).get - a
            val expected = as - a
            actual <-> expected
          }
        },
        test("size") {
          check(genSet) { as =>
            val actual   = NonEmptySortedSet.fromSetOption(as).get.size
            val expected = as.size
            actual <-> expected
          }
        },
      ),
      suite("constructors")(
        test("fromSetOption") {
          check(genSet) { as =>
            val nonEmptySet = NonEmptySortedSet.fromSetOption(as).get
            val set         = nonEmptySet.toSet
            assert(set)(equalTo(as))
          }
        }
      )
    )
}
