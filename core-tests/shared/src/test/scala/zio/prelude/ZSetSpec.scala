package zio.prelude

import zio.Chunk
import zio.prelude.Associative._
import zio.prelude.Equal._
import zio.prelude.ZSet._
import zio.prelude.coherent.{CovariantDeriveEqual, DeriveEqualForEach}
import zio.prelude.laws._
import zio.prelude.newtypes.{Natural, _}
import zio.random.Random
import zio.test.Assertion.{equalTo => _, _}
import zio.test._
import zio.test.laws._

object ZSetSpec extends DefaultRunnableSpec {

  def genFZSet[R <: Random with Sized, B](b: Gen[R, B]): GenF[R, ({ type lambda[+x] = ZSet[x, B] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ZSet[x, B] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, ZSet[A, B]] =
        genZSet(a, b)
    }

  def genZSet[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, ZSet[A, B]] =
    Gen.mapOf(a, b).map(ZSet.fromMap)

  lazy val smallInts: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  def natural(min: Natural, max: Natural): Gen[Random, Natural] =
    Gen.int(min, max).map(_.asInstanceOf[Natural])

  def naturals: Gen[Random with Sized, Natural] =
    Gen.small(n => natural(0.asInstanceOf[Natural], n.asInstanceOf[Natural]))

  implicit def SumIdentity[A: Identity]: Identity[Sum[A]] =
    Identity[A].invmap(Equivalence(Sum.wrap, Sum.unwrap))

  def spec: ZSpec[Environment, Failure] =
    suite("ZSetSpec")(
      suite("laws")(
        testM("combine commutative")(
          checkAllLaws(CommutativeLaws)(genZSet(Gen.anyInt, Gen.anyInt).map(_.transform(Sum(_))))
        ),
        testM("covariant")(
          checkAllLaws[
            CovariantDeriveEqual,
            Equal,
            TestConfig,
            Random with Sized with TestConfig,
            ({ type lambda[+x] = ZSet[x, Int] })#lambda,
            Int
          ](CovariantLaws)(genFZSet(Gen.anyInt), Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            CovariantDeriveEqual.derive[({ type lambda[+x] = ZSet[x, Int] })#lambda](
              ZSetCovariant(IntSumCommutativeInverse),
              ZSetDeriveEqual(IntHashOrd, Identity[Sum[Int]])
            ),
            IntHashOrd
          )
        ),
        testM("foreach")(
          checkAllLaws[
            DeriveEqualForEach,
            Equal,
            TestConfig,
            Random with Sized with TestConfig,
            MultiSet,
            Int
          ](ForEachLaws)(genFZSet(naturals), Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            DeriveEqualForEach.derive[MultiSet](
              ZSetDeriveEqual(IntHashOrd, Identity[Sum[Natural]]),
              MultiSetForEach
            ),
            IntHashOrd
          )
        ),
        testM("hash")(checkAllLaws(HashLaws)(genZSet(Gen.anyInt, Gen.anyInt)))
      ),
      suite("methods")(
        test("zipWith") {
          val die  = ZSet(1, 2, 3, 4, 5, 6)
          val pair = die.zipWith(die)(_ + _)
          assert(pair(7))(equalTo(6))
        }
      ),
      suite("set")(
        testM("diff") {
          check(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)) { (l, r) =>
            val actual   = (ZSet.fromSet(l) &~ ZSet.fromSet(r)).toSet
            val expected = l &~ r
            assert(actual)(equalTo(expected))
          }
        },
        testM("flatMap") {
          check(Gen.setOf(Gen.anyInt), Gen.function(Gen.setOf(Gen.anyInt))) { (as, f) =>
            val actual   = ZSet.fromSet(as).flatMap(a => ZSet.fromSet(f(a))).toSet
            val expected = as.flatMap(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("intersect") {
          check(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)) { (l, r) =>
            val actual   = (ZSet.fromSet(l) & ZSet.fromSet(r)).toSet
            val expected = l & r
            assert(actual)(equalTo(expected))
          }
        },
        testM("union") {
          check(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)) { (l, r) =>
            val actual   = (ZSet.fromSet(l) | ZSet.fromSet(r)).toSet
            val expected = l | r
            assert(actual)(equalTo(expected))
          }
        }
      ),
      suite("constructors")(
        test("fromMap") {
          import scala.collection.immutable.SortedMap
          trait Unordered
          trait Ordered extends Unordered
          object Ordered {
            implicit val ord: scala.math.Ordering[Ordered] =
              new scala.math.Ordering[Ordered] {
                def compare(l: Ordered, r: Ordered): Int = 0
              }
          }
          val unordered = new Unordered {}
          val ordered   = new Ordered {}
          val sortedMap = SortedMap(ordered -> true)
          val set       = ZSet.fromMap(sortedMap)
          assert(set(unordered))(isFalse)
        }
      )
    )
}
