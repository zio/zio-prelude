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
        testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
        testM("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
        testM("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyInt))),
        testM("double multiplication")(checkAllLaws(Identity)(Gen.anyDouble.map(Prod(_)))),
        testM("duration ZIO")(checkAllLaws(Identity)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        testM("float multiplication")(checkAllLaws(Identity)(Gen.anyFloat.map(Prod(_)))),
        testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
        testM("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyInt))),
        testM("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
        testM("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        testM("option")(checkAllLaws(Identity)(Gen.option(Gen.anyInt.map(Sum(_))))),
        testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
        testM("string")(checkAllLaws(Identity)(Gen.anyString)),
        testM("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
