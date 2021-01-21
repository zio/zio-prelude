package zio.prelude

import zio.prelude.newtypes._
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object IdentitySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

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
        testM("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyInt))),
        testM("double max")(checkAllLaws(Identity)(Gen.anyDouble.map(Max(_)))),
        testM("double min")(checkAllLaws(Identity)(Gen.anyDouble.map(Min(_)))),
        testM("double multiplication")(checkAllLaws(Identity)(Gen.anyDouble.map(Prod(_)))),
        testM("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        testM("float max")(checkAllLaws(Identity)(Gen.anyFloat.map(Max(_)))),
        testM("float min")(checkAllLaws(Identity)(Gen.anyFloat.map(Min(_)))),
        testM("float multiplication")(checkAllLaws(Identity)(Gen.anyFloat.map(Prod(_)))),
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
