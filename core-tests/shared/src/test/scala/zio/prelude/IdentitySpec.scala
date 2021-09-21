package zio.prelude

import zio.prelude.laws.IdentityLaws
import zio.prelude.newtypes._
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object IdentitySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("IdentitySpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(Or(_)))),
        test("byte multiplication")(checkAllLaws(IdentityLaws)(Gen.anyByte.map(Prod(_)))),
        test("char multiplication")(checkAllLaws(IdentityLaws)(Gen.anyChar.map(Prod(_)))),
        test("chunk")(checkAllLaws(IdentityLaws)(Gen.chunkOf(Gen.anyInt))),
        test("double multiplication")(checkAllLaws(IdentityLaws)(Gen.anyDouble.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(IdentityLaws)(Gen.anyFiniteDuration)),
        test("either")(checkAllLaws(IdentityLaws)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        test("float multiplication")(checkAllLaws(IdentityLaws)(Gen.anyFloat.map(Prod(_)))),
        test("int multiplication")(checkAllLaws(IdentityLaws)(Gen.anyInt.map(Prod(_)))),
        test("list")(checkAllLaws(IdentityLaws)(Gen.listOf(Gen.anyInt))),
        test("long multiplication")(checkAllLaws(IdentityLaws)(Gen.anyLong.map(Prod(_)))),
        test("map")(checkAllLaws(IdentityLaws)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        test("option")(checkAllLaws(IdentityLaws)(Gen.option(Gen.anyInt.map(Sum(_))))),
        test("short multiplication")(checkAllLaws(IdentityLaws)(Gen.anyShort.map(Prod(_)))),
        test("string")(checkAllLaws(IdentityLaws)(Gen.anyString)),
        test("vector")(checkAllLaws(IdentityLaws)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
