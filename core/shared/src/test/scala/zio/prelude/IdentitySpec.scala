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
        test("boolean conjuction")(checkAllLaws(Identity)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(Identity)(Gen.boolean.map(Or(_)))),
        test("byte max")(checkAllLaws(Identity)(Gen.anyByte.map(Max(_)))),
        test("byte min")(checkAllLaws(Identity)(Gen.anyByte.map(Min(_)))),
        test("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
        test("char max")(checkAllLaws(Identity)(Gen.anyChar.map(Max(_)))),
        test("char min")(checkAllLaws(Identity)(Gen.anyChar.map(Min(_)))),
        test("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
        test("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyInt))),
        test("double max")(checkAllLaws(Identity)(Gen.anyDouble.map(Max(_)))),
        test("double min")(checkAllLaws(Identity)(Gen.anyDouble.map(Min(_)))),
        test("double multiplication")(checkAllLaws(Identity)(Gen.anyDouble.map(Prod(_)))),
        test("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        test("float max")(checkAllLaws(Identity)(Gen.anyFloat.map(Max(_)))),
        test("float min")(checkAllLaws(Identity)(Gen.anyFloat.map(Min(_)))),
        test("float multiplication")(checkAllLaws(Identity)(Gen.anyFloat.map(Prod(_)))),
        test("int max")(checkAllLaws(Identity)(Gen.anyInt.map(Max(_)))),
        test("int min")(checkAllLaws(Identity)(Gen.anyInt.map(Min(_)))),
        test("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
        test("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyInt))),
        test("long max")(checkAllLaws(Identity)(Gen.anyLong.map(Max(_)))),
        test("long min")(checkAllLaws(Identity)(Gen.anyLong.map(Min(_)))),
        test("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
        test("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        test("option")(checkAllLaws(Identity)(Gen.option(Gen.anyInt.map(Sum(_))))),
        test("short max")(checkAllLaws(Identity)(Gen.anyShort.map(Max(_)))),
        test("short min")(checkAllLaws(Identity)(Gen.anyShort.map(Min(_)))),
        test("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
        test("string")(checkAllLaws(Identity)(Gen.anyString)),
        test("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
