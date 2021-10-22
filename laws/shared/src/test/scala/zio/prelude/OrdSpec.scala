package zio.prelude

import zio.ZIO
import zio.prelude.Common.anyFiniteDurationScala
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object OrdSpec extends DefaultRunnableSpec {

  def sign(n: Int): Int =
    if (n > 0) 1 else if (n < 0) -1 else 0

  def scalaOrderingConsistency[R, A: Ord](
    gen: Gen[R, A]
  )(implicit ord: scala.math.Ordering[A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(a1 =?= a2)(equalTo(Ordering.fromCompare(ord.compare(a1, a2)))) &&
      assert(sign(Ord[A].toScala.compare(a1, a2)))(equalTo(sign(ord.compare(a1, a2))))
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
        testM("boolean")(checkAllLaws(OrdLaws)(Gen.boolean)),
        testM("byte")(checkAllLaws(OrdLaws)(Gen.anyByte)),
        testM("char")(checkAllLaws(OrdLaws)(Gen.anyChar)),
        testM("chunk")(checkAllLaws(OrdLaws)(Gen.chunkOf(Gen.anyInt))),
        testM("double")(checkAllLaws(OrdLaws)(Gen.anyDouble)),
        testM("duration Scala")(checkAllLaws(OrdLaws)(anyFiniteDurationScala)),
        testM("duration ZIO")(checkAllLaws(OrdLaws)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(OrdLaws)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("float")(checkAllLaws(OrdLaws)(Gen.anyFloat)),
        testM("int")(checkAllLaws(OrdLaws)(Gen.anyInt)),
        testM("list")(checkAllLaws(OrdLaws)(Gen.listOf(Gen.anyInt))),
        testM("long")(checkAllLaws(OrdLaws)(Gen.anyLong)),
        testM("option")(checkAllLaws(OrdLaws)(Gen.option(Gen.anyInt))),
        testM("string")(checkAllLaws(OrdLaws)(Gen.anyString)),
        testM("tuple2")(checkAllLaws(OrdLaws)(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(checkAllLaws(OrdLaws)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        testM("unit")(checkAllLaws(OrdLaws)(Gen.unit)),
        testM("vector")(checkAllLaws(OrdLaws)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaOrdering consistency")(
        testM("unit")(scalaOrderingConsistency(Gen.unit)),
        testM("boolean")(scalaOrderingConsistency(Gen.boolean)),
        testM("byte")(scalaOrderingConsistency(Gen.anyByte)),
        testM("char")(scalaOrderingConsistency(Gen.anyChar)),
        testM("duration Scala")(scalaOrderingConsistency(anyFiniteDurationScala)),
        testM("duration ZIO")(scalaOrderingConsistency(Gen.anyFiniteDuration)),
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
