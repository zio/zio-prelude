package zio.prelude
package experimental

import zio.prelude._
import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object AnnihilationSpec extends ZIOBaseSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()

  def spec: Spec[Environment, Any] =
    suite("AnnihilationSpec")(
      suite("laws")(
        test("double annihilating")(checkAllLaws(AnnihilationLaws)(Gen.double)),
        test("int annihilating")(checkAllLaws(AnnihilationLaws)(Gen.int))
      )
    )
}
