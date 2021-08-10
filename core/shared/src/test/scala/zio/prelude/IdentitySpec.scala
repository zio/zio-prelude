package zio.prelude

import zio.prelude.newtypes._
import zio.test._
import zio.test.laws._

object IdentitySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("IdentitySpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(Identity)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(Identity)(Gen.boolean.map(Or(_)))),
        test("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
        test("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
        test("chunk")(checkAllLaws(Identity)(Gen.chunkOf(Gen.anyInt))),
        test("double multiplication")(checkAllLaws(Identity)(Gen.anyDouble.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(Identity)(Gen.anyFiniteDuration)),
        test("either")(checkAllLaws(Identity)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        test("float multiplication")(checkAllLaws(Identity)(Gen.anyFloat.map(Prod(_)))),
        test("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
        test("list")(checkAllLaws(Identity)(Gen.listOf(Gen.anyInt))),
        test("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_)))),
        test("map")(checkAllLaws(Identity)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        test("option")(checkAllLaws(Identity)(Gen.option(Gen.anyInt.map(Sum(_))))),
        test("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
        test("string")(checkAllLaws(Identity)(Gen.anyString)),
        test("vector")(checkAllLaws(Identity)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
