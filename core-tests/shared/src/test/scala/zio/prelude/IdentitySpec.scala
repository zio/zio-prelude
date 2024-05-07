package zio.prelude

import zio.prelude.laws.IdentityLaws
import zio.prelude.newtypes._
import zio.test._
import zio.test.laws._

object IdentitySpec extends ZIOBaseSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: Spec[Environment, Any] =
    suite("IdentitySpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(Or(_)))),
        test("byte multiplication")(checkAllLaws(IdentityLaws)(Gen.byte.map(Prod(_)))),
        test("char multiplication")(checkAllLaws(IdentityLaws)(Gen.char.map(Prod(_)))),
        test("chunk")(checkAllLaws(IdentityLaws)(Gen.chunkOf(Gen.int))),
        test("double multiplication")(checkAllLaws(IdentityLaws)(Gen.double.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(IdentityLaws)(Gen.finiteDuration)),
        test("either")(checkAllLaws(IdentityLaws)(Gen.either(Gen.int.map(Sum(_)), Gen.int.map(Sum(_))))),
        test("float multiplication")(checkAllLaws(IdentityLaws)(Gen.float.map(Prod(_)))),
        test("int multiplication")(checkAllLaws(IdentityLaws)(Gen.int.map(Prod(_)))),
        test("list")(checkAllLaws(IdentityLaws)(Gen.listOf(Gen.int))),
        test("long multiplication")(checkAllLaws(IdentityLaws)(Gen.long.map(Prod(_)))),
        test("map")(checkAllLaws(IdentityLaws)(Gen.mapOf(Gen.int, Gen.int.map(Sum(_))))),
        test("option")(checkAllLaws(IdentityLaws)(Gen.option(Gen.int.map(Sum(_))))),
        test("short multiplication")(checkAllLaws(IdentityLaws)(Gen.short.map(Prod(_)))),
        test("string")(checkAllLaws(IdentityLaws)(Gen.string)),
        test("vector")(checkAllLaws(IdentityLaws)(Gen.vectorOf(Gen.int)))
      )
    )
}
