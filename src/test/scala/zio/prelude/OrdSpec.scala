package zio.prelude

import zio.ZIO
import zio.test._
import zio.test.laws._

object OrdSpec extends DefaultRunnableSpec {

  def sign(n: Int): Int =
    if (n > 0) 1 else if (n < 0) -1 else 0

  def scalaOrderingConsistency[R, A: Ord](
    gen: Gen[R, A]
  )(implicit ord: scala.math.Ordering[A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(a1 =?= a2)(isEqualTo(Ordering.fromCompare(ord.compare(a1, a2)))) &&
      assert(sign(Ord[A].toScala.compare(a1, a2)))(isEqualTo(sign(ord.compare(a1, a2))))
    }

  /*
   * Copied from Ordering.Implicits to resolve incompatibility in implicits
   * organization between 2.12 and 2.13.
   */
  implicit def iterableOrdering[CC[A] <: Iterable[A], A](implicit
    ord: scala.math.Ordering[A]
  ): scala.math.Ordering[CC[A]] =
    new scala.math.Ordering[CC[A]] {
      def compare(x: CC[A], y: CC[A]): Int = {
        val xit = x.iterator
        val yit = y.iterator

        while (xit.hasNext && yit.hasNext) {
          val res = ord.compare(xit.next(), yit.next())
          if (res !== 0) return res
        }

        scala.math.Ordering[Boolean].compare(xit.hasNext, yit.hasNext)
      }
    }

  def spec: ZSpec[Environment, Failure] =
    suite("OrdSpec")(
      suite("laws")(
        testM("unit")(checkAllLaws(Ord)(Gen.unit)),
        testM("boolean")(checkAllLaws(Ord)(Gen.boolean)),
        testM("byte")(checkAllLaws(Ord)(Gen.anyByte)),
        testM("char")(checkAllLaws(Ord)(Gen.anyChar)),
        testM("string")(checkAllLaws(Ord)(Gen.anyString)),
        testM("int")(checkAllLaws(Ord)(Gen.anyInt)),
        testM("long")(checkAllLaws(Ord)(Gen.anyLong)),
        testM("float")(checkAllLaws(Ord)(Gen.anyFloat)),
        testM("double")(checkAllLaws(Ord)(Gen.anyDouble)),
        testM("option")(checkAllLaws(Ord)(Gen.option(Gen.anyInt))),
        testM("either")(checkAllLaws(Ord)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("tuple2")(checkAllLaws(Ord)(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(checkAllLaws(Ord)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        testM("list")(checkAllLaws(Ord)(Gen.listOf(Gen.anyInt))),
        testM("vector")(checkAllLaws(Ord)(Gen.vectorOf(Gen.anyInt))),
        testM("chunk")(checkAllLaws(Ord)(Gen.chunkOf(Gen.anyInt)))
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
