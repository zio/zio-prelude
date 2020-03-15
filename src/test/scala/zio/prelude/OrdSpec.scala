package zio.prelude

import zio.test._
import zio.test.laws._
import zio.test.Assertion.isTrue
import zio.ZIO

object OrdSpec extends DefaultRunnableSpec {

  final def scalaOrderingConsistency[R, A: Ord](
    gen: Gen[R, A]
  )(implicit ord: scala.math.Ordering[A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert((a1 =?= a2) === Ordering.fromCompare(ord.compare(a1, a2)))(isTrue)
    }

  implicit def scalaListOrdering[A: scala.math.Ordering]: scala.math.Ordering[List[A]] =
    scala.math.Ordering.Iterable[A].on(identity)

  implicit def scalaVectorOrdering[A: scala.math.Ordering]: scala.math.Ordering[Vector[A]] =
    scala.math.Ordering.Iterable[A].on(identity)

  def spec = suite("OrdSpec")(
    suite("laws")(
      testM("boolean")(checkAllLaws(Equal)(Gen.boolean)),
      testM("byte")(checkAllLaws(Equal)(Gen.anyByte)),
      testM("char")(checkAllLaws(Equal)(Gen.anyChar)),
      testM("double")(checkAllLaws(Equal)(Gen.anyDouble)),
      testM("either")(checkAllLaws(Equal)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("float")(checkAllLaws(Equal)(Gen.anyFloat)),
      testM("int")(checkAllLaws(Equal)(Gen.anyInt)),
      testM("list")(checkAllLaws(Equal)(Gen.listOf(Gen.anyInt))),
      testM("long")(checkAllLaws(Equal)(Gen.anyLong)),
      testM("option")(checkAllLaws(Equal)(Gen.option(Gen.anyInt))),
      testM("string")(checkAllLaws(Equal)(Gen.anyString)),
      testM("tuple")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt))),
      testM("unit")(checkAllLaws(Equal)(Gen.unit)),
      testM("vector")(checkAllLaws(Equal)(Gen.vectorOf(Gen.anyInt)))
    ),
    suite("ScalaOrdering consistency")(
      testM("boolean")(scalaOrderingConsistency(Gen.boolean)),
      testM("byte")(scalaOrderingConsistency(Gen.anyByte)),
      testM("char")(scalaOrderingConsistency(Gen.anyChar)),
      testM("int")(scalaOrderingConsistency(Gen.anyInt)),
      testM("list")(scalaOrderingConsistency(Gen.listOf(Gen.anyInt))),
      testM("long")(scalaOrderingConsistency(Gen.anyLong)),
      testM("option")(scalaOrderingConsistency(Gen.option(Gen.anyInt))),
      testM("string")(scalaOrderingConsistency(Gen.anyString)),
      testM("tuple")(scalaOrderingConsistency(Gen.anyInt.zip(Gen.anyInt))),
      testM("unit")(scalaOrderingConsistency(Gen.unit)),
      testM("vector")(scalaOrderingConsistency(Gen.vectorOf(Gen.anyInt)))
    )
  )
}
