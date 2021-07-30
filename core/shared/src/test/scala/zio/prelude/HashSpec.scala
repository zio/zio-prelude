package zio.prelude

import zio.test._
import zio.test.laws._
import zio.{Has, ZIO}

object HashSpec extends DefaultRunnableSpec {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with Has[TestConfig], Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Failure] =
    suite("HashSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(Hash)(Gen.boolean)),
        test("byte")(checkAllLaws(Hash)(Gen.anyByte)),
        test("char")(checkAllLaws(Hash)(Gen.anyChar)),
        test("chunk")(checkAllLaws(Hash)(Gen.chunkOf(Gen.anyInt))),
        test("double")(checkAllLaws(Hash)(Gen.anyDouble)),
        test("either")(checkAllLaws(Hash)(Gen.either(Gen.anyInt, Gen.anyInt))),
        test("float")(checkAllLaws(Hash)(Gen.anyFloat)),
        test("int")(checkAllLaws(Hash)(Gen.anyInt)),
        test("list")(checkAllLaws(Hash)(Gen.listOf(Gen.anyInt))),
        test("long")(checkAllLaws(Hash)(Gen.anyLong)),
        test("map")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        test("option")(checkAllLaws(Hash)(Gen.option(Gen.anyInt))),
        test("set")(checkAllLaws(Hash)(Gen.setOf(Gen.anyInt))),
        test("string")(checkAllLaws(Hash)(Gen.anyString)),
        test("tuple2")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyInt))),
        test("tuple3")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        test("unit")(checkAllLaws(Hash)(Gen.unit)),
        test("vector")(checkAllLaws(Hash)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaHashCode consistency")(
        test("unit")(scalaHashCodeConsistency(Gen.unit)),
        test("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        test("byte")(scalaHashCodeConsistency(Gen.anyByte)),
        test("char")(scalaHashCodeConsistency(Gen.anyChar)),
        test("string")(scalaHashCodeConsistency(Gen.anyString)),
        test("int")(scalaHashCodeConsistency(Gen.anyInt)),
        test("long")(scalaHashCodeConsistency(Gen.anyLong)),
        test("float")(scalaHashCodeConsistency(Gen.anyFloat)),
        test("double")(scalaHashCodeConsistency(Gen.anyDouble)),
        test("option")(scalaHashCodeConsistency(Gen.option(Gen.anyInt))),
        test("tuple2")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyInt))),
        test("tuple3")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        test("either")(scalaHashCodeConsistency(Gen.either(Gen.anyInt, Gen.anyInt))),
        test("list")(scalaHashCodeConsistency(Gen.listOf(Gen.anyInt))),
        test("vector")(scalaHashCodeConsistency(Gen.vectorOf(Gen.anyInt))),
        test("set")(scalaHashCodeConsistency(Gen.setOf(Gen.anyInt))),
        test("map")(scalaHashCodeConsistency(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        test("chunk")(scalaHashCodeConsistency(Gen.chunkOf(Gen.anyInt)))
      )
    )
}
