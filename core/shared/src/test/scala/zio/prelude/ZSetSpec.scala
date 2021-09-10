package zio.prelude

import zio.prelude.Associative._
import zio.prelude.Equal._
import zio.prelude.ZSet._
import zio.prelude.coherent.{CovariantDeriveEqual, DeriveEqualForEach}
import zio.prelude.newtypes.{Natural, _}
import zio.test.Assertion._
import zio.test._
import zio.test.laws._
import zio.{Chunk, Has, Random}

object ZSetSpec extends DefaultRunnableSpec {

  def genFZSet[R <: Has[Random] with Has[Sized], B](b: Gen[R, B]): GenF[R, ({ type lambda[+x] = ZSet[x, B] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ZSet[x, B] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, ZSet[A, B]] =
        genZSet(a, b)
    }

  def genZSet[R <: Has[Random] with Has[Sized], A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, ZSet[A, B]] =
    Gen.mapOf(a, b).map(ZSet.fromMap)

  lazy val smallInts: Gen[Has[Random] with Has[Sized], Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  def natural(min: Natural, max: Natural): Gen[Random, Natural] =
    Gen.int(min, max).map(_.asInstanceOf[Natural])

  def naturals: Gen[Has[Random] with Has[Sized], Natural] =
    Gen.small(n => natural(0.asInstanceOf[Natural], n.asInstanceOf[Natural]))

  implicit def SumIdentity[A: Identity]: Identity[Sum[A]] =
    Identity[A].invmap(Equivalence(Sum.wrap, Sum.unwrap))

  def spec: ZSpec[Environment, Failure] =
    suite("ZSetSpec")(
      suite("laws")(
        test("combine commutative")(
          checkAllLaws(Commutative)(genZSet(Gen.anyInt, Gen.anyInt).map(_.transform(Sum(_))))
        ),
        test("covariant")(
          checkAllLaws[
            CovariantDeriveEqual,
            Equal,
            Has[TestConfig],
            Has[Random] with Has[Sized] with Has[TestConfig],
            ({ type lambda[+x] = ZSet[x, Int] })#lambda,
            Int
          ](Covariant)(genFZSet(Gen.anyInt), Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            CovariantDeriveEqual.derive[({ type lambda[+x] = ZSet[x, Int] })#lambda](
              ZSetCovariant(IntSumCommutativeInverse),
              ZSetDeriveEqual(IntHashOrd, Identity[Sum[Int]])
            ),
            IntHashOrd
          )
        ),
        test("foreach")(
          checkAllLaws[
            DeriveEqualForEach,
            Equal,
            Has[TestConfig],
            Has[Random] with Has[Sized] with Has[TestConfig],
            MultiSet,
            Int
          ](ForEach)(genFZSet(naturals), Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            DeriveEqualForEach.derive[MultiSet](
              ZSetDeriveEqual(IntHashOrd, Identity[Sum[Natural]]),
              MultiSetForEach
            ),
            IntHashOrd
          )
        ),
        test("hash")(checkAllLaws(Hash)(genZSet(Gen.anyInt, Gen.anyInt)))
      ),
      suite("methods")(
        test("zipWith") {
          val die  = ZSet(1, 2, 3, 4, 5, 6)
          val pair = die.zipWith(die)(_ + _)
          assert(pair(7))(equalTo(6))
        }
      ),
      suite("set")(
        test("diff") {
          check(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)) { (l, r) =>
            val actual   = (ZSet.fromSet(l) &~ ZSet.fromSet(r)).toSet
            val expected = l &~ r
            assert(actual)(equalTo(expected))
          }
        },
        test("flatMap") {
          check(Gen.setOf(Gen.anyInt), Gen.function(Gen.setOf(Gen.anyInt))) { (as, f) =>
            val actual   = ZSet.fromSet(as).flatMap(a => ZSet.fromSet(f(a))).toSet
            val expected = as.flatMap(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("intersect") {
          check(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)) { (l, r) =>
            val actual   = (ZSet.fromSet(l) & ZSet.fromSet(r)).toSet
            val expected = l & r
            assert(actual)(equalTo(expected))
          }
        },
        test("union") {
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
