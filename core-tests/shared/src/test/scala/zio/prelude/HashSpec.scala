package zio.prelude

import zio.ZIO
import zio.prelude.Common.anyFiniteDurationScala
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object HashSpec extends DefaultRunnableSpec {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Failure] =
    suite("HashSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(HashLaws)(Gen.boolean)),
        testM("byte")(checkAllLaws(HashLaws)(Gen.anyByte)),
        testM("char")(checkAllLaws(HashLaws)(Gen.anyChar)),
        testM("chunk")(checkAllLaws(HashLaws)(Gen.chunkOf(Gen.anyInt))),
        testM("double")(checkAllLaws(HashLaws)(Gen.anyDouble)),
        testM("duration Scala")(checkAllLaws(HashLaws)(anyFiniteDurationScala)),
        testM("duration ZIO")(checkAllLaws(HashLaws)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(HashLaws)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("float")(checkAllLaws(HashLaws)(Gen.anyFloat)),
        testM("int")(checkAllLaws(HashLaws)(Gen.anyInt)),
        testM("list")(checkAllLaws(HashLaws)(Gen.listOf(Gen.anyInt))),
        testM("long")(checkAllLaws(HashLaws)(Gen.anyLong)),
        testM("map")(checkAllLaws(HashLaws)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        testM("option")(checkAllLaws(HashLaws)(Gen.option(Gen.anyInt))),
        testM("set")(checkAllLaws(HashLaws)(Gen.setOf(Gen.anyInt))),
        testM("string")(checkAllLaws(HashLaws)(Gen.anyString)),
        testM("tuple2")(checkAllLaws(HashLaws)(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(checkAllLaws(HashLaws)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        testM("unit")(checkAllLaws(HashLaws)(Gen.unit)),
        testM("vector")(checkAllLaws(HashLaws)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaHashCode consistency")(
        testM("unit")(scalaHashCodeConsistency(Gen.unit)),
        testM("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        testM("byte")(scalaHashCodeConsistency(Gen.anyByte)),
        testM("char")(scalaHashCodeConsistency(Gen.anyChar)),
        testM("duration Scala")(scalaHashCodeConsistency(anyFiniteDurationScala)),
        testM("duration ZIO")(scalaHashCodeConsistency(Gen.anyFiniteDuration)),
        testM("string")(scalaHashCodeConsistency(Gen.anyString)),
        testM("int")(scalaHashCodeConsistency(Gen.anyInt)),
        testM("long")(scalaHashCodeConsistency(Gen.anyLong)),
        testM("float")(scalaHashCodeConsistency(Gen.anyFloat)),
        testM("double")(scalaHashCodeConsistency(Gen.anyDouble)),
        testM("option")(scalaHashCodeConsistency(Gen.option(Gen.anyInt))),
        testM("tuple2")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        testM("either")(scalaHashCodeConsistency(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("list")(scalaHashCodeConsistency(Gen.listOf(Gen.anyInt))),
        testM("vector")(scalaHashCodeConsistency(Gen.vectorOf(Gen.anyInt))),
        testM("set")(scalaHashCodeConsistency(Gen.setOf(Gen.anyInt))),
        testM("map")(scalaHashCodeConsistency(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        testM("chunk")(scalaHashCodeConsistency(Gen.chunkOf(Gen.anyInt)))
      )
    )
}
