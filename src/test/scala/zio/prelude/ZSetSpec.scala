package zio.prelude

import zio.Chunk
import zio.prelude.Commutative._
import zio.prelude.Equal._
import zio.prelude.ZSet._
import zio.prelude.coherent.CovariantDeriveEqual
import zio.prelude.newtypes._
import zio.random.Random
import zio.test.Assertion._
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

  val smallInts: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  def spec = suite("ZSetSpec")(
    suite("laws")(
      testM("combine commutative")(
        checkAllLaws(Commutative)(genZSet(Gen.anyInt, Gen.anyInt).map(_.transform(Sum(_))))
      ),
      testM("covariant")(
        checkAllLaws[
          CovariantDeriveEqual,
          Equal,
          Any,
          Random with Sized,
          ({ type lambda[+x] = ZSet[x, Int] })#lambda,
          Int
        ](Covariant)(genFZSet(Gen.anyInt), Gen.anyInt)(
          // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
          CovariantDeriveEqual.derive[({ type lambda[+x] = ZSet[x, Int] })#lambda](
            ZSetCovariant(IntSumCommutative),
            ZSetDeriveEqual(IntEqual)
          ),
          IntEqual
        )
      ),
      testM("equal")(checkAllLaws(Equal)(genZSet(Gen.anyInt, Gen.anyInt))),
      testM("hash")(checkAllLaws(Hash)(genZSet(Gen.anyInt, Gen.anyInt))),
      testM("intersect commutative")(
        checkAllLaws(Commutative)(genZSet(Gen.anyInt, Gen.anyInt).map(_.transform(Min(_))))
      ),
      testM("union commutative")(
        checkAllLaws(Commutative)(genZSet(Gen.anyInt, Gen.anyInt).map(_.transform(Max(_))))
      )
    ),
    suite("methods")(
      test("flatMap") {
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
