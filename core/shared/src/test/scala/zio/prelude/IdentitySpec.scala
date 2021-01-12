package zio.prelude

import zio.prelude.newtypes._
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object IdentitySpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentitySpec")(
      suite("laws")(
        testM("boolean conjuction")(checkAllLaws(Identity)(Gen.boolean.map(And(_)))),
        testM("boolean disjunction")(checkAllLaws(Identity)(Gen.boolean.map(Or(_)))),
        testM("byte max")(checkAllLaws(Identity)(Gen.anyByte.map(Max(_)))),
        testM("byte min")(checkAllLaws(Identity)(Gen.anyByte.map(Min(_)))),
        testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
        testM("char max")(checkAllLaws(Identity)(Gen.anyChar.map(Max(_)))),
        testM("char min")(checkAllLaws(Identity)(Gen.anyChar.map(Min(_)))),
        testM("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
        testM("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        testM("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyInt))),
        testM("int max")(checkAllLaws(Identity)(Gen.anyInt.map(Max(_)))),
        testM("int min")(checkAllLaws(Identity)(Gen.anyInt.map(Min(_)))),
        testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
        testM("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyInt))),
        testM("long max")(checkAllLaws(Identity)(Gen.anyLong.map(Max(_)))),
        testM("long min")(checkAllLaws(Identity)(Gen.anyLong.map(Min(_)))),
        testM("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
        testM("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        testM("option")(checkAllLaws(Identity)(Gen.option(Gen.anyInt.map(Sum(_))))),
        testM("short max")(checkAllLaws(Identity)(Gen.anyShort.map(Max(_)))),
        testM("short min")(checkAllLaws(Identity)(Gen.anyShort.map(Min(_)))),
        testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
        testM("string")(checkAllLaws(Identity)(Gen.anyString)),
        testM("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
