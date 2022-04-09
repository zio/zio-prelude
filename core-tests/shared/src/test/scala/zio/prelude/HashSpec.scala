package zio.prelude

import zio.ZIO
import zio.prelude.Common.finiteDurationScala
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object HashSpec extends ZIOSpecDefault {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Nothing] =
    suite("HashSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(HashLaws)(Gen.boolean)),
        test("byte")(checkAllLaws(HashLaws)(Gen.byte)),
        test("char")(checkAllLaws(HashLaws)(Gen.char)),
        test("chunk")(checkAllLaws(HashLaws)(Gen.chunkOf(Gen.int))),
        test("double")(checkAllLaws(HashLaws)(Gen.double)),
        test("duration Scala")(checkAllLaws(HashLaws)(finiteDurationScala)),
        test("duration ZIO")(checkAllLaws(HashLaws)(Gen.finiteDuration)),
        test("either")(checkAllLaws(HashLaws)(Gen.either(Gen.int, Gen.int))),
        test("float")(checkAllLaws(HashLaws)(Gen.float)),
        test("int")(checkAllLaws(HashLaws)(Gen.int)),
        test("list")(checkAllLaws(HashLaws)(Gen.listOf(Gen.int))),
        test("long")(checkAllLaws(HashLaws)(Gen.long)),
        test("map")(checkAllLaws(HashLaws)(Gen.mapOf(Gen.int, Gen.int))),
        test("option")(checkAllLaws(HashLaws)(Gen.option(Gen.int))),
        test("set")(checkAllLaws(HashLaws)(Gen.setOf(Gen.int))),
        test("string")(checkAllLaws(HashLaws)(Gen.string)),
        test("tuple2")(checkAllLaws(HashLaws)(Gen.int.zip(Gen.int))),
        test("tuple3")(checkAllLaws(HashLaws)(Gen.int.zip(Gen.int).zip(Gen.int))),
        test("unit")(checkAllLaws(HashLaws)(Gen.unit)),
        test("vector")(checkAllLaws(HashLaws)(Gen.vectorOf(Gen.int)))
      ),
      suite("ScalaHashCode consistency")(
        test("unit")(scalaHashCodeConsistency(Gen.unit)),
        test("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        test("byte")(scalaHashCodeConsistency(Gen.byte)),
        test("char")(scalaHashCodeConsistency(Gen.char)),
        test("duration Scala")(scalaHashCodeConsistency(finiteDurationScala)),
        test("duration ZIO")(scalaHashCodeConsistency(Gen.finiteDuration)),
        test("string")(scalaHashCodeConsistency(Gen.string)),
        test("int")(scalaHashCodeConsistency(Gen.int)),
        test("long")(scalaHashCodeConsistency(Gen.long)),
        test("float")(scalaHashCodeConsistency(Gen.float)),
        test("double")(scalaHashCodeConsistency(Gen.double)),
        test("option")(scalaHashCodeConsistency(Gen.option(Gen.int))),
        test("tuple2")(scalaHashCodeConsistency(Gen.int.zip(Gen.int))),
        test("tuple3")(scalaHashCodeConsistency(Gen.int.zip(Gen.int).zip(Gen.int))),
        test("either")(scalaHashCodeConsistency(Gen.either(Gen.int, Gen.int))),
        test("list")(scalaHashCodeConsistency(Gen.listOf(Gen.int))),
        test("vector")(scalaHashCodeConsistency(Gen.vectorOf(Gen.int))),
        test("set")(scalaHashCodeConsistency(Gen.setOf(Gen.int))),
        test("map")(scalaHashCodeConsistency(Gen.mapOf(Gen.int, Gen.int))),
        test("chunk")(scalaHashCodeConsistency(Gen.chunkOf(Gen.int)))
      )
    )
}
