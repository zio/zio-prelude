package zio.prelude

import zio.test._
import zio.test.Assertion._
import zio.ZIO

object OrdSpec extends DefaultRunnableSpec {

  final def ordLaws[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    for {
      a <- transitivityLaw1(gen)
      b <- transitivityLaw2(gen)
      c <- antisymmetryLaw1(gen)
      d <- antisymmetryLaw2(gen)
      e <- connexityLaw1(gen)
      f <- connexityLaw2(gen)
      g <- complementLaw(gen)
    } yield a && b && c && d && e && f && g

  final def transitivityLaw1[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen, gen) { (a1, a2, a3) =>
      assert(Ord[A].transitivityLaw1(a1, a2, a3))(isTrue ?? "transitivityLaw1")
    }

  final def transitivityLaw2[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen, gen) { (a1, a2, a3) =>
      assert(Ord[A].transitivityLaw2(a1, a2, a3))(isTrue ?? "transitivityLaw2")
    }

  final def antisymmetryLaw1[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(Ord[A].antisymmetryLaw1(a1, a2))(isTrue ?? "antisymmetryLaw1")
    }

  final def antisymmetryLaw2[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(Ord[A].antisymmetryLaw2(a1, a2))(isTrue ?? "antisymmetryLaw2")
    }

  final def connexityLaw1[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(Ord[A].connexityLaw1(a1, a2))(isTrue ?? "connexityLaw1")
    }

  final def connexityLaw2[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(Ord[A].connexityLaw2(a1, a2))(isTrue ?? "connexityLaw2")
    }

  final def complementLaw[R, A: Ord](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert(Ord[A].complementLaw(a1, a2))(isTrue ?? "complementLaw")
    }

  def spec = suite("OrdSpec")(
    suite("laws")(
      testM("boolean")(ordLaws(Gen.boolean)),
      testM("byte")(ordLaws(Gen.anyByte)),
      testM("char")(ordLaws(Gen.anyChar)),
      testM("double")(ordLaws(Gen.anyDouble)),
      testM("either")(ordLaws(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("float")(ordLaws(Gen.anyFloat)),
      testM("int")(ordLaws(Gen.anyInt)),
      testM("list")(ordLaws(Gen.listOf(Gen.anyInt))),
      testM("long")(ordLaws(Gen.anyLong)),
      testM("option")(ordLaws(Gen.option(Gen.anyInt))),
      testM("string")(ordLaws(Gen.anyString)),
      testM("tuple")(ordLaws(Gen.anyInt.zip(Gen.anyInt))),
      testM("unit")(ordLaws(Gen.unit)),
      testM("vector")(ordLaws(Gen.vectorOf(Gen.anyInt)))
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
}
