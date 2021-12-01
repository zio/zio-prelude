package zio.prelude

import zio.prelude.Associative._
import zio.prelude.Equal._
import zio.prelude.ZNonEmptySet._
import zio.prelude.coherent.CovariantDeriveEqual
import zio.prelude.laws._
import zio.prelude.newtypes._
import zio.test._
import zio.test.laws._
import zio.{Chunk, Random, ZTraceElement}

object ZNonEmptySetSpec extends DefaultRunnableSpec {

  def genFZNonEmptySet[R <: Random with Sized, B](
    b: Gen[R, B]
  ): GenF[R, ({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, ZNonEmptySet[A, B]] =
        genZNonEmptySet(a, b)
    }

  def genZNonEmptySet[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, ZNonEmptySet[A, B]] =
    Gen.mapOf1(a, b).map(ZNonEmptySet.fromMapOption(_).get)

  val smallInts: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  implicit def SumIdentity[A: Identity]: Identity[Sum[A]] =
    Identity[A].invmap(Equivalence(Sum.wrap, Sum.unwrap))

  def spec: ZSpec[Environment, Failure] =
    suite("ZNonEmptySetSpec")(
      suite("laws")(
        test("combine commutative")(
          checkAllLaws(CommutativeLaws)(genZNonEmptySet(Gen.int, Gen.int).map(_.transform(Sum(_))))
        ),
        test("covariant")(
          checkAllLaws[
            CovariantDeriveEqual,
            Equal,
            TestConfig,
            Random with Sized with TestConfig,
            ({ type lambda[+x] = ZNonEmptySet[x, Int] })#lambda,
            Int
          ](CovariantLaws)(genFZNonEmptySet(Gen.int), Gen.int)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            CovariantDeriveEqual.derive[({ type lambda[+x] = ZNonEmptySet[x, Int] })#lambda](
              ZNonEmptySetCovariant(IntSumCommutativeInverse),
              ZNonEmptySetDeriveEqual(IntHashOrd, Identity[Sum[Int]])
            ),
            IntHashOrd,
            implicitly[ZTraceElement]
          )
        ),
        test("hash")(checkAllLaws(HashLaws)(genZNonEmptySet(Gen.int, Gen.int)))
      ),
      suite("methods")(
        test("zipWith") {
          val die  = ZNonEmptySet(1, 2, 3, 4, 5, 6)
          val pair = die.zipWith(die)(_ + _)
          assert(pair(7))(equalTo(6))
        },
        test("peelNonEmpty none") {
          val znes     = ZNonEmptySet("a")
          val destruct = znes.peelNonEmpty
          assert(destruct)(equalTo(("a", None)))
        },
        test("peelNonEmpty some 1") {
          val znes     = ZNonEmptySet("a", "b")
          val destruct = znes.peelNonEmpty
          assert(destruct)(equalTo(("a", Some(ZNonEmptySet("b")))))
        },
        test("peelNonEmpty some 2") {
          val znes     = ZNonEmptySet("a", "a")
          val destruct = znes.peelNonEmpty
          assert(destruct)(equalTo(("a", Some(ZNonEmptySet("a")))))
        }
      ),
      suite("set")(
        test("flatMap") {
          check(Gen.setOf1(Gen.int), Gen.function(Gen.setOf1(Gen.int))) { (as, f) =>
            val actual   = ZNonEmptySet.fromSetOption(as).get.flatMap(a => ZNonEmptySet.fromSetOption(f(a)).get).toSet
            val expected = as.flatMap(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("union") {
          check(Gen.setOf1(Gen.int), Gen.setOf1(Gen.int)) { (l, r) =>
            val actual   = (ZNonEmptySet.fromSetOption(l).get | ZNonEmptySet.fromSetOption(r).get).toSet
            val expected = l | r
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
