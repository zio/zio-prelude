package zio.prelude

import zio.prelude.newtypes.{ And, Or, Prod, Sum }
import zio.test._
import zio.test.laws._
import zio.test.DefaultRunnableSpec

object IdentitySpec extends DefaultRunnableSpec {
  def spec = suite("IdentitySpec")(
    suite("laws")(
      testM("char addition")(checkAllLaws(Identity)(Gen.anyChar.map(Sum(_)))),
      testM("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
      testM("string")(checkAllLaws(Identity)(Gen.anyString)),
      testM("byte addition")(checkAllLaws(Identity)(Gen.anyByte.map(Sum(_)))),
      testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
      testM("short addition")(checkAllLaws(Identity)(Gen.anyShort.map(Sum(_)))),
      testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
      testM("int addition")(checkAllLaws(Identity)(Gen.anyInt.map(Sum(_)))),
      testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
      testM("long addition")(checkAllLaws(Identity)(Gen.anyLong.map(Sum(_)))),
      testM("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
      testM("boolean disjunction")(checkAllLaws(Identity)(Gen.boolean.map(Or(_)))),
      testM("boolean conjuction")(checkAllLaws(Identity)(Gen.boolean.map(And(_)))),
      testM("option")(checkAllLaws(Identity)(Gen.option(Gen.anyString))),
      testM("either")(checkAllLaws(Identity)(Gen.either(Gen.anyString, Gen.anyString))),
      testM("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyString))),
      testM("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyString))),
      testM("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyString, Gen.anyString))),
      testM("set")(checkAllLaws(Identity)(Gen.setOf(Gen.anyString))),
      testM("tuple2")(checkAllLaws(Identity)(Gen.anyString.zip(Gen.anyString))),
      testM("tuple3")(checkAllLaws(Identity)(Gen.anyString.zip(Gen.anyString).zip(Gen.anyString).map {
        case ((a, b), c) => (a, b, c)
      })),
      testM("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyString)))
    )
  )
}
