package zio.prelude

import zio.prelude.newtypes.{ And, Max, Min, Or, Prod, Sum }
import zio.test.laws._
import zio.test.{ DefaultRunnableSpec, _ }

object IdentitySpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("IdentitySpec")(
      suite("laws")(
        testM("char addition")(checkAllLaws(Identity)(Gen.anyChar.map(Sum(_)))),
        testM("char max")(checkAllLaws(Identity)(Gen.anyChar.map(Max(_)))),
        testM("char min")(checkAllLaws(Identity)(Gen.anyChar.map(Min(_)))),
        testM("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
        testM("string")(checkAllLaws(Identity)(Gen.anyString)),
        testM("byte addition")(checkAllLaws(Identity)(Gen.anyByte.map(Sum(_)))),
        testM("byte max")(checkAllLaws(Identity)(Gen.anyByte.map(Max(_)))),
        testM("byte min")(checkAllLaws(Identity)(Gen.anyByte.map(Min(_)))),
        testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
        testM("short addition")(checkAllLaws(Identity)(Gen.anyShort.map(Sum(_)))),
        testM("short max")(checkAllLaws(Identity)(Gen.anyShort.map(Max(_)))),
        testM("short min")(checkAllLaws(Identity)(Gen.anyShort.map(Min(_)))),
        testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
        testM("int addition")(checkAllLaws(Identity)(Gen.anyInt.map(Sum(_)))),
        testM("int max")(checkAllLaws(Identity)(Gen.anyInt.map(Max(_)))),
        testM("int min")(checkAllLaws(Identity)(Gen.anyInt.map(Min(_)))),
        testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
        testM("long addition")(checkAllLaws(Identity)(Gen.anyLong.map(Sum(_)))),
        testM("long max")(checkAllLaws(Identity)(Gen.anyLong.map(Max(_)))),
        testM("long min")(checkAllLaws(Identity)(Gen.anyLong.map(Min(_)))),
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
