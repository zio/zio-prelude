package zio.prelude

import zio.test._
import zio.test.laws._
import zio.test.DefaultRunnableSpec

object IdentitySpec extends DefaultRunnableSpec {
  def spec = suite("IdentitySpec")(
    suite("laws")(
      testM("char")(checkAllLaws(Identity)(Gen.anyChar)),
      testM("string")(checkAllLaws(Identity)(Gen.anyString)),
      testM("byte addition")(checkAllLaws(Identity)(Gen.anyByte.map(Sum(_)))),
      testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
      testM("short addition")(checkAllLaws(Identity)(Gen.anyShort.map(Sum(_)))),
      testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
      testM("int addition")(checkAllLaws(Identity)(Gen.anyInt.map(Sum(_)))),
      testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
      testM("long addition")(checkAllLaws(Identity)(Gen.anyLong.map(Sum(_)))),
      testM("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
      testM("boolean disjunction")(checkAllLaws(Identity)(Gen.boolean.map(Disj(_)))),
      testM("boolean conjuction")(checkAllLaws(Identity)(Gen.boolean.map(Conj(_)))),
      // testM("option")(checkAllLaws(Identity)(Gen.option(Gen.anyInt))),
      // testM("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyInt))),
      testM("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyInt))),
      // testM("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
      testM("set")(checkAllLaws(Identity)(Gen.setOf(Gen.anyInt))),
      // testM("tuple2")(checkAllLaws(Identity)(Gen.anyInt.zip(Gen.anyString))),
      // testM("tuple3")(checkAllLaws(Identity)(Gen.anyInt.zip(Gen.anyString).zip(Gen.anyString)))
    )
  )
}
