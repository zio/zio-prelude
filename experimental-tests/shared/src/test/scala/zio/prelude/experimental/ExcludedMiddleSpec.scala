package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object ExcludedMiddleSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("ExcludedMiddleSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(ExcludedMiddleLaws)(Gen.boolean))
      )
    )
}
