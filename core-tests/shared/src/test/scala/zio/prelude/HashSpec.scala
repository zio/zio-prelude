package zio.prelude

import zio.prelude.Common.anyFiniteDurationScala
import zio.prelude.laws._
import zio.test._
import zio.test.laws._
import zio.{Has, ZIO}

object HashSpec extends DefaultRunnableSpec {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with Has[TestConfig], Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Failure] =
    suite("HashSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(HashLaws)(Gen.boolean)),
        test("byte")(checkAllLaws(HashLaws)(Gen.anyByte)),
        test("char")(checkAllLaws(HashLaws)(Gen.anyChar)),
        test("chunk")(checkAllLaws(HashLaws)(Gen.chunkOf(Gen.anyInt))),
        test("double")(checkAllLaws(HashLaws)(Gen.anyDouble)),
        test("duration Scala")(checkAllLaws(HashLaws)(anyFiniteDurationScala)),
        test("duration ZIO")(checkAllLaws(HashLaws)(Gen.anyFiniteDuration)),
        test("either")(checkAllLaws(HashLaws)(Gen.either(Gen.anyInt, Gen.anyInt))),
        test("float")(checkAllLaws(HashLaws)(Gen.anyFloat)),
        test("int")(checkAllLaws(HashLaws)(Gen.anyInt)),
        test("list")(checkAllLaws(HashLaws)(Gen.listOf(Gen.anyInt))),
        test("long")(checkAllLaws(HashLaws)(Gen.anyLong)),
        test("map")(checkAllLaws(HashLaws)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        test("option")(checkAllLaws(HashLaws)(Gen.option(Gen.anyInt))),
        test("set")(checkAllLaws(HashLaws)(Gen.setOf(Gen.anyInt))),
        test("string")(checkAllLaws(HashLaws)(Gen.anyString)),
        test("tuple2")(checkAllLaws(HashLaws)(Gen.anyInt.zip(Gen.anyInt))),
        test("tuple3")(checkAllLaws(HashLaws)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        test("unit")(checkAllLaws(HashLaws)(Gen.unit)),
        test("vector")(checkAllLaws(HashLaws)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaHashCode consistency")(
        test("unit")(scalaHashCodeConsistency(Gen.unit)),
        test("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        test("byte")(scalaHashCodeConsistency(Gen.anyByte)),
        test("char")(scalaHashCodeConsistency(Gen.anyChar)),
        test("duration Scala")(scalaHashCodeConsistency(anyFiniteDurationScala)),
        test("duration ZIO")(scalaHashCodeConsistency(Gen.anyFiniteDuration)),
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
