package zio.prelude

import zio.Chunk
import zio.prelude.Associative._
import zio.prelude.Equal._
import zio.prelude.ZNonEmptySet._
import zio.prelude.coherent.CovariantDeriveEqual
import zio.prelude.newtypes._
import zio.random.Random
import zio.test._
import zio.test.laws._

object ZNonEmptySetSpec extends DefaultRunnableSpec {

  def genFZNonEmptySet[R <: Random with Sized, B](
    b: Gen[R, B]
  ): GenF[R, ({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, ZNonEmptySet[A, B]] =
        genZNonEmptySet(a, b)
    }

  def genZNonEmptySet[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, ZNonEmptySet[A, B]] =
    Gen.mapOf1(a, b).map(ZNonEmptySet.fromMapOption(_).get)

  val smallInts: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  def spec: ZSpec[Environment, Failure] =
    suite("ZNonEmptySetSpec")(
      suite("laws")(
        testM("combine commutative")(
          checkAllLaws(Commutative)(genZNonEmptySet(Gen.anyInt, Gen.anyInt).map(_.transform(Sum(_))))
        ),
        testM("covariant")(
          checkAllLaws[
            CovariantDeriveEqual,
            Equal,
            TestConfig,
            Random with Sized with TestConfig,
            ({ type lambda[+x] = ZNonEmptySet[x, Int] })#lambda,
            Int
          ](Covariant)(genFZNonEmptySet(Gen.anyInt), Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            CovariantDeriveEqual.derive[({ type lambda[+x] = ZNonEmptySet[x, Int] })#lambda](
              ZNonEmptySetCovariant(IntSumCommutativeInverse),
              ZNonEmptySetDeriveEqual(IntHashOrd)
            ),
            IntHashOrd
          )
        ),
        testM("equal")(checkAllLaws(Equal)(genZNonEmptySet(Gen.anyInt, Gen.anyInt))),
        testM("hash")(checkAllLaws(Hash)(genZNonEmptySet(Gen.anyInt, Gen.anyInt))),
        testM("intersect commutative")(
          checkAllLaws(Commutative)(genZNonEmptySet(Gen.anyInt, Gen.anyInt).map(_.transform(Min(_))))
        ),
        testM("union commutative")(
          checkAllLaws(Commutative)(genZNonEmptySet(Gen.anyInt, Gen.anyInt).map(_.transform(Max(_))))
        )
      ),
      suite("methods")(
        test("zipWith") {
          val die  = ZNonEmptySet(1, 2, 3, 4, 5, 6)
          val pair = die.zipWith(die)(_ + _)
          assert(pair(7))(equalTo(6))
        }
      ),
      suite("set")(
        testM("flatMap") {
          check(Gen.setOf1(Gen.anyInt), Gen.function(Gen.setOf1(Gen.anyInt))) { (as, f) =>
            val actual   = ZNonEmptySet.fromSetOption(as).get.flatMap(a => ZNonEmptySet.fromSetOption(f(a)).get).toSet
            val expected = as.flatMap(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("union") {
          check(Gen.setOf1(Gen.anyInt), Gen.setOf1(Gen.anyInt)) { (l, r) =>
            val actual   = (ZNonEmptySet.fromSetOption(l).get | ZNonEmptySet.fromSetOption(r).get).toSet
            val expected = l | r
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
