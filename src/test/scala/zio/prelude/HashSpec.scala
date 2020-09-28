package zio.prelude

import zio.ZIO
import zio.test._
import zio.test.laws._

object HashSpec extends DefaultRunnableSpec {

  final def scalaHashCodeConsistency[R, A: Hash](gen: Gen[R, A]): ZIO[R with TestConfig, Nothing, TestResult] =
    check(gen)(a => assert(a.hash)(equalTo(a.hashCode)))

  def spec: ZSpec[Environment, Failure] =
    suite("HashSpec")(
      suite("laws")(
        testM("unit")(checkAllLaws(Hash)(Gen.unit)),
        testM("boolean")(checkAllLaws(Hash)(Gen.boolean)),
        testM("byte")(checkAllLaws(Hash)(Gen.anyByte)),
        testM("char")(checkAllLaws(Hash)(Gen.anyChar)),
        testM("string")(checkAllLaws(Hash)(Gen.anyString)),
        testM("int")(checkAllLaws(Hash)(Gen.anyInt)),
        testM("long")(checkAllLaws(Hash)(Gen.anyLong)),
        testM("float")(checkAllLaws(Hash)(Gen.anyFloat)),
        testM("double")(checkAllLaws(Hash)(Gen.anyDouble)),
        testM("option")(checkAllLaws(Hash)(Gen.option(Gen.anyInt))),
        testM("tuple2")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyString))),
        testM("tuple3")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyString).zip(Gen.anyString))),
        testM("either")(checkAllLaws(Hash)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("list")(checkAllLaws(Hash)(Gen.listOf(Gen.anyInt))),
        testM("vector")(checkAllLaws(Hash)(Gen.vectorOf(Gen.anyInt))),
        testM("set")(checkAllLaws(Hash)(Gen.setOf(Gen.anyInt))),
        testM("map")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        testM("chunk")(checkAllLaws(Hash)(Gen.chunkOf(Gen.anyInt)))
      ),
      suite("ScalaHashCode consistency")(
        testM("unit")(scalaHashCodeConsistency(Gen.unit)),
        testM("boolean")(scalaHashCodeConsistency(Gen.boolean)),
        testM("byte")(scalaHashCodeConsistency(Gen.anyByte)),
        testM("char")(scalaHashCodeConsistency(Gen.anyChar)),
        testM("string")(scalaHashCodeConsistency(Gen.anyString)),
        testM("int")(scalaHashCodeConsistency(Gen.anyInt)),
        testM("long")(scalaHashCodeConsistency(Gen.anyLong)),
        testM("float")(scalaHashCodeConsistency(Gen.anyFloat)),
        testM("double")(scalaHashCodeConsistency(Gen.anyDouble)),
        testM("option")(scalaHashCodeConsistency(Gen.option(Gen.anyInt))),
        testM("tuple2")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyString))),
        testM("tuple3")(scalaHashCodeConsistency(Gen.anyInt.zip(Gen.anyString).zip(Gen.anyString))),
        testM("either")(scalaHashCodeConsistency(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("list")(scalaHashCodeConsistency(Gen.listOf(Gen.anyInt))),
        testM("vector")(scalaHashCodeConsistency(Gen.vectorOf(Gen.anyInt))),
        testM("set")(scalaHashCodeConsistency(Gen.setOf(Gen.anyInt))),
        testM("map")(scalaHashCodeConsistency(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
        testM("chunk")(scalaHashCodeConsistency(Gen.chunkOf(Gen.anyInt)))
      )
    )
}
