package zio.prelude
package experimental

import zio.prelude._
import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object AnnihilationSpec extends ZIOBaseSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: Spec[Environment, Any] =
    suite("AnnihilationSpec")(
      suite("laws")(
        test("BigDecimal annihilating")(
          checkAllLaws(AnnihilationLaws)(Gen.bigDecimal(BigDecimal(-10), BigDecimal(10)))
        ),
        test("byte annihilating")(checkAllLaws(AnnihilationLaws)(Gen.byte)),
        test("char annihilating")(checkAllLaws(AnnihilationLaws)(Gen.char)),
        test("double annihilating")(checkAllLaws(AnnihilationLaws)(Gen.double)),
        test("float annihilating")(checkAllLaws(AnnihilationLaws)(Gen.float)),
        test("int annihilating")(checkAllLaws(AnnihilationLaws)(Gen.int)),
        test("long annihilating")(checkAllLaws(AnnihilationLaws)(Gen.long)),
        test("short annihilating")(checkAllLaws(AnnihilationLaws)(Gen.short))
      )
    )
}
