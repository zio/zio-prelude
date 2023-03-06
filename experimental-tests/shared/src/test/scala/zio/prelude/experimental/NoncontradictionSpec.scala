package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object NoncontradictionSpec extends ZIOBaseSpec {

  def spec: Spec[Environment, Any] =
    suite("NoncontradictionSpec")(
      suite("laws")(
        test("boolean")(checkAllLaws(NoncontradictionLaws)(Gen.boolean))
      )
    )
}
