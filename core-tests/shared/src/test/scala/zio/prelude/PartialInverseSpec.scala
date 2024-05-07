package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes.Prod
import zio.test._
import zio.test.laws._

object PartialInverseSpec extends ZIOBaseSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: Spec[Environment, Any] =
    suite("PartialInverseSpec")(
      suite("laws")(
        test("BigDecimal prod")(
          checkAllLaws(PartialInverseLaws)(Gen.bigDecimal(BigDecimal(-10), BigDecimal(10)).map(Prod(_)))
        ),
        test("byte prod")(checkAllLaws(PartialInverseLaws)(Gen.byte.map(Prod(_)))),
        test("char prod")(checkAllLaws(PartialInverseLaws)(Gen.char.map(Prod(_)))),
        test("double prod")(checkAllLaws(PartialInverseLaws)(Gen.double.map(Prod(_)))),
        test("float prod")(checkAllLaws(PartialInverseLaws)(Gen.float.map(Prod(_)))),
        test("int prod")(checkAllLaws(PartialInverseLaws)(Gen.int.map(Prod(_)))),
        test("long prod")(checkAllLaws(PartialInverseLaws)(Gen.long.map(Prod(_)))),
        test("short prod")(checkAllLaws(PartialInverseLaws)(Gen.short.map(Prod(_))))
      )
    )
}
