package zio.prelude

import zio.prelude.Common.finiteDurationScala
import zio.prelude.laws._
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
        test("boolean")(checkAllLaws(OrdLaws)(Gen.boolean)),
        test("byte")(checkAllLaws(OrdLaws)(Gen.byte)),
        test("char")(checkAllLaws(OrdLaws)(Gen.char)),
        test("chunk")(checkAllLaws(OrdLaws)(Gen.chunkOf(Gen.int))),
        test("double")(checkAllLaws(OrdLaws)(Gen.double)),
        test("duration Scala")(checkAllLaws(OrdLaws)(finiteDurationScala)),
        test("duration ZIO")(checkAllLaws(OrdLaws)(Gen.finiteDuration)),
        test("either")(checkAllLaws(OrdLaws)(Gen.either(Gen.int, Gen.int))),
        test("float")(checkAllLaws(OrdLaws)(Gen.float)),
        test("int")(checkAllLaws(OrdLaws)(Gen.int)),
        test("list")(checkAllLaws(OrdLaws)(Gen.listOf(Gen.int))),
        test("long")(checkAllLaws(OrdLaws)(Gen.long)),
        test("option")(checkAllLaws(OrdLaws)(Gen.option(Gen.int))),
        test("string")(checkAllLaws(OrdLaws)(Gen.string)),
        test("tuple2")(checkAllLaws(OrdLaws)(Gen.int.zip(Gen.int))),
        test("tuple3")(checkAllLaws(OrdLaws)(Gen.int.zip(Gen.int).zip(Gen.int))),
        test("unit")(checkAllLaws(OrdLaws)(Gen.unit)),
        test("vector")(checkAllLaws(OrdLaws)(Gen.vectorOf(Gen.int)))
      ),
      suite("ScalaOrdering consistency")(
        test("unit")(scalaOrderingConsistency(Gen.unit)),
        test("boolean")(scalaOrderingConsistency(Gen.boolean)),
        test("byte")(scalaOrderingConsistency(Gen.byte)),
        test("char")(scalaOrderingConsistency(Gen.char)),
        test("duration Scala")(scalaOrderingConsistency(finiteDurationScala)),
        test("duration ZIO")(scalaOrderingConsistency(Gen.finiteDuration)),
        test("string")(scalaOrderingConsistency(Gen.string)),
        test("int")(scalaOrderingConsistency(Gen.int)),
        test("long")(scalaOrderingConsistency(Gen.long)),
        test("option")(scalaOrderingConsistency(Gen.option(Gen.int))),
        test("tuple2")(scalaOrderingConsistency(Gen.int.zip(Gen.int))),
        test("tuple3")(scalaOrderingConsistency(Gen.int.zip(Gen.int).zip(Gen.int))),
        test("list")(scalaOrderingConsistency(Gen.listOf(Gen.int))),
        test("vector")(scalaOrderingConsistency(Gen.vectorOf(Gen.int)))
      )
    )
}
