package zio.prelude

import zio.ZIO
import zio.prelude.Common.anyFiniteDurationScala
import zio.test._
import zio.test.laws._

object HashSpec extends DefaultRunnableSpec {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Failure] =
    suite("HashSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(Hash)(Gen.boolean)),
        testM("byte")(checkAllLaws(Hash)(Gen.anyByte)),
        testM("char")(checkAllLaws(Hash)(Gen.anyChar)),
        testM("chunk")(checkAllLaws(Hash)(Gen.chunkOf(Gen.anyInt))),
        testM("double")(checkAllLaws(Hash)(Gen.anyDouble)),
        testM("duration ZIO")(checkAllLaws(Hash)(Gen.anyFiniteDuration)),
        testM("duration Scala")(checkAllLaws(Hash)(anyFiniteDurationScala)),
        testM("either")(checkAllLaws(Hash)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("float")(checkAllLaws(Hash)(Gen.anyFloat)),
        testM("int")(checkAllLaws(Hash)(Gen.anyInt)),
        testM("list")(checkAllLaws(Hash)(Gen.listOf(Gen.anyInt))),
        testM("long")(checkAllLaws(Hash)(Gen.anyLong)),
        testM("map")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        testM("option")(checkAllLaws(Hash)(Gen.option(Gen.anyInt))),
        testM("set")(checkAllLaws(Hash)(Gen.setOf(Gen.anyInt))),
        testM("string")(checkAllLaws(Hash)(Gen.anyString)),
        testM("tuple2")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
        testM("unit")(checkAllLaws(Hash)(Gen.unit)),
        testM("vector")(checkAllLaws(Hash)(Gen.vectorOf(Gen.anyInt)))
      ),
      suite("ScalaHashCode consistency")(
        testM("unit")(scalaHashCodeConsistency(Gen.unit)),
        testM("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        testM("byte")(scalaHashCodeConsistency(Gen.anyByte)),
        testM("char")(scalaHashCodeConsistency(Gen.anyChar)),
        testM("duration ZIO")(scalaHashCodeConsistency(Gen.anyFiniteDuration)),
        testM("duration Scala")(scalaHashCodeConsistency(anyFiniteDurationScala)),
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
