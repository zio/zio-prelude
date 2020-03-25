package zio.prelude

import zio.test._
import zio.test.laws._

import zio.test.DefaultRunnableSpec

object HashSpec extends DefaultRunnableSpec {
  def spec = suite("HashSpec")(
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
      testM("map")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt)))
    )
  )
}
