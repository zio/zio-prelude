package zio.prelude

import zio.Chunk
import zio.prelude.newtypes._
import zio.random.Random
import zio.test._
import zio.test.laws._

object MultiSetSpec extends DefaultRunnableSpec {

  def genFMultiSet[R <: Random with Sized, A](a: Gen[R, A]): GenF[R, ({ type lambda[+x] = MultiSet[A, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = MultiSet[A, x] })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B]): Gen[R1, MultiSet[A, B]] =
        genMultiSet(a, b)
    }

  def genMultiSet[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, MultiSet[A, B]] =
    Gen.mapOf(a, b).map(MultiSet(_))

  val smallInts: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(Gen.int(-10, 10))

  def spec = suite("MultiSetSpec")(
    suite("laws")(
      testM("combine commutative")(
        checkAllLaws(Commutative)(genMultiSet(Gen.anyInt, Gen.anyInt).map(_.transform(Sum(_))))
      ),
      testM("equal")(checkAllLaws(Equal)(genMultiSet(Gen.anyInt, Gen.anyInt))),
      testM("hash")(checkAllLaws(Hash)(genMultiSet(Gen.anyInt, Gen.anyInt))),
      testM("intersect commutative")(
        checkAllLaws(Commutative)(genMultiSet(Gen.anyInt, Gen.anyInt).map(_.transform(Min(_))))
      ),
      testM("union commutative")(
        checkAllLaws(Commutative)(genMultiSet(Gen.anyInt, Gen.anyInt).map(_.transform(Max(_))))
      )
    ),
    suite("methods")(
      test("flatMap") {
        val die  = MultiSet(1, 2, 3, 4, 5, 6)
        val pair = die.zipWith(die)(_ + _)
        assert(pair(7))(equalTo(6))
      }
    ),
    suite("constructors")()
  )
}
