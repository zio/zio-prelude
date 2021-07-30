package zio.prelude

import zio.test._
import zio.test.laws._
import zio.{Has, ZIO}

object OrdSpec extends DefaultRunnableSpec {

  def sign(n: Int): Int =
    if (n > 0) 1 else if (n < 0) -1 else 0

  def scalaOrderingConsistency[R, A: Ord](
    gen: Gen[R, A]
  )(implicit ord: scala.math.Ordering[A]): ZIO[R with Has[TestConfig], Nothing, TestResult] =
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
        test("boolean")(checkAllLaws(Ord)(Gen.boolean)),
        test("byte")(checkAllLaws(Ord)(Gen.anyByte)),
        test("char")(checkAllLaws(Ord)(Gen.anyChar)),
        test("chunk")(checkAllLaws(Ord)(Gen.chunkOf(Gen.anyInt))),
        test("double")(checkAllLaws(Ord)(Gen.anyDouble)),
        test("either")(checkAllLaws(Ord)(Gen.either(Gen.anyInt, Gen.anyInt))),
        test("float")(checkAllLaws(Ord)(Gen.anyFloat)),
        test("int")(checkAllLaws(Ord)(Gen.anyInt)),
        test("list")(checkAllLaws(Ord)(Gen.listOf(Gen.anyInt))),
        test("long")(checkAllLaws(Ord)(Gen.anyLong)),
        test("option")(checkAllLaws(Ord)(Gen.option(Gen.anyInt))),
        test("string")(checkAllLaws(Ord)(Gen.anyString)),
        test("tuple2")(checkAllLaws(Ord)(Gen.anyInt.zip(Gen.anyInt))),
        test("tuple3")(checkAllLaws(Ord)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        test("unit")(checkAllLaws(Ord)(Gen.unit)),
        test("vector")(checkAllLaws(Ord)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaOrdering consistency")(
        test("unit")(scalaOrderingConsistency(Gen.unit)),
        test("boolean")(scalaOrderingConsistency(Gen.boolean)),
        test("byte")(scalaOrderingConsistency(Gen.anyByte)),
        test("char")(scalaOrderingConsistency(Gen.anyChar)),
        test("string")(scalaOrderingConsistency(Gen.anyString)),
        test("int")(scalaOrderingConsistency(Gen.anyInt)),
        test("long")(scalaOrderingConsistency(Gen.anyLong)),
        test("option")(scalaOrderingConsistency(Gen.option(Gen.anyInt))),
        test("tuple2")(scalaOrderingConsistency(Gen.anyInt.zip(Gen.anyInt))),
        test("tuple3")(scalaOrderingConsistency(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        test("list")(scalaOrderingConsistency(Gen.listOf(Gen.anyInt))),
        test("vector")(scalaOrderingConsistency(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
