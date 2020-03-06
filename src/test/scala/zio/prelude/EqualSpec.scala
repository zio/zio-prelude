package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.Assertion._
import zio.ZIO

object EqualSpec extends DefaultRunnableSpec {

  final def equalLaws[R, A: Equal](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    for {
      a <- reflexiveLaw(gen)
      b <- symmetryLaw(gen)
      c <- transitivityLaw(gen)
    } yield a && b && c

  final def reflexiveLaw[R, A: Equal](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen) { a =>
      assert(a === a)(isTrue ?? "reflexiveLaw")
    }

  final def symmetryLaw[R, A: Equal](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert((a1 === a2) ==> (a2 === a1))(isTrue ?? "symmetryLaw")
    }

  final def transitivityLaw[R, A: Equal](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen, gen) { (a1, a2, a3) =>
      assert(((a1 === a2) && (a2 === a3)) ==> (a1 === a3))(isTrue ?? "transitivityLaw")
    }

  def spec = suite("EqualSpec")(
    suite("laws")(
      testM("boolean")(equalLaws(Gen.boolean)),
      testM("byte")(equalLaws(Gen.anyByte)),
      testM("char")(equalLaws(Gen.anyChar)),
      testM("double")(equalLaws(Gen.anyDouble)),
      testM("either")(equalLaws(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("float")(equalLaws(Gen.anyFloat)),
      testM("int")(equalLaws(Gen.anyInt)),
      testM("list")(equalLaws(Gen.listOf(Gen.anyInt))),
      testM("long")(equalLaws(Gen.anyLong)),
      testM("map")(equalLaws(anyMap(Gen.anyInt, Gen.anyInt))),
      testM("option")(equalLaws(Gen.option(Gen.anyInt))),
      testM("set")(equalLaws(anySet(Gen.anyInt))),
      testM("string")(equalLaws(Gen.anyString)),
      testM("tuple")(equalLaws(Gen.anyInt.zip(Gen.anyInt))),
      testM("unit")(equalLaws(Gen.unit)),
      testM("vector")(equalLaws(Gen.vectorOf(Gen.anyInt)))
    ),
    test("DoubleEqual correctly handles `Double.NaN") {
      assert(Equal[Double].equal(Double.NaN, Double.NaN))(isTrue)
    },
    test("FloatEqual  correctly handles `Float.NaN") {
      assert(Equal[Float].equal(Float.NaN, Float.NaN))(isTrue)
    }
  )

  def anyMap[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, Map[A, B]] =
    Gen.listOf(a.zip(b)).map(_.toMap)

  def anySet[R <: Random with Sized, A](gen: Gen[R, A]): Gen[R, Set[A]] =
    Gen.listOf(gen).map(_.toSet)
}
