package zio.prelude

import zio.test._
import zio.test.laws._
import zio.ZIO

object OrdSpec extends DefaultRunnableSpec {

  final def scalaOrderingConsistency[R, A: Ord](
    gen: Gen[R, A]
  )(implicit ord: scala.math.Ordering[A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(a1 =?= a2)(equalTo(Ordering.fromCompare(ord.compare(a1, a2))))
    }

  /*
   * Copied from Ordering.Implicits to resolve incompatibility in implicits
   * organization between 2.12 and 2.13.
   */
  implicit def iterableOrdering[CC[A] <: Iterable[A], A](
    implicit ord: scala.math.Ordering[A]
  ): scala.math.Ordering[CC[A]] =
    new scala.math.Ordering[CC[A]] {
      def compare(x: CC[A], y: CC[A]): Int = {
        val xit = x.iterator
        val yit = y.iterator

        while (xit.hasNext && yit.hasNext) {
          val res = ord.compare(xit.next(), yit.next())
          if (res != 0) return res
        }

        scala.math.Ordering[Boolean].compare(xit.hasNext, yit.hasNext)
      }
    }

  def spec = suite("OrdSpec")(
    suite("laws")(
      testM("unit")(checkAllLaws(Equal)(Gen.unit)),
      testM("boolean")(checkAllLaws(Equal)(Gen.boolean)),
      testM("byte")(checkAllLaws(Equal)(Gen.anyByte)),
      testM("char")(checkAllLaws(Equal)(Gen.anyChar)),
      testM("string")(checkAllLaws(Equal)(Gen.anyString)),
      testM("int")(checkAllLaws(Equal)(Gen.anyInt)),
      testM("long")(checkAllLaws(Equal)(Gen.anyLong)),
      testM("float")(checkAllLaws(Equal)(Gen.anyFloat)),
      testM("double")(checkAllLaws(Equal)(Gen.anyDouble)),
      testM("option")(checkAllLaws(Equal)(Gen.option(Gen.anyInt))),
      testM("either")(checkAllLaws(Equal)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("tuple2")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt))),
      testM("tuple3")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
      testM("list")(checkAllLaws(Equal)(Gen.listOf(Gen.anyInt))),
      testM("vector")(checkAllLaws(Equal)(Gen.vectorOf(Gen.anyInt))),
      testM("chunk")(checkAllLaws(Equal)(Gen.chunkOf(Gen.anyInt)))
    ),
    suite("ScalaOrdering consistency")(
      testM("unit")(scalaOrderingConsistency(Gen.unit)),
      testM("boolean")(scalaOrderingConsistency(Gen.boolean)),
      testM("byte")(scalaOrderingConsistency(Gen.anyByte)),
      testM("char")(scalaOrderingConsistency(Gen.anyChar)),
      testM("string")(scalaOrderingConsistency(Gen.anyString)),
      testM("int")(scalaOrderingConsistency(Gen.anyInt)),
      testM("long")(scalaOrderingConsistency(Gen.anyLong)),
      testM("option")(scalaOrderingConsistency(Gen.option(Gen.anyInt))),
      testM("tuple2")(scalaOrderingConsistency(Gen.anyInt.zip(Gen.anyInt))),
      testM("tuple3")(scalaOrderingConsistency(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
      testM("list")(scalaOrderingConsistency(Gen.listOf(Gen.anyInt))),
      testM("vector")(scalaOrderingConsistency(Gen.vectorOf(Gen.anyInt)))
    )
  )
}
