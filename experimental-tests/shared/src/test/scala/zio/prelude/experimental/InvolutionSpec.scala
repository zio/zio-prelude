package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object InvolutionSpec extends ZIOBaseSpec {

  def spec: Spec[Environment, Any] =
    suite("InvolutionSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(InvolutionLaws)(Gen.boolean))
      )
    )
}
