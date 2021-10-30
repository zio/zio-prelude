package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object NoncontradictionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("NoncontradictionSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(NoncontradictionLaws)(Gen.boolean))
      )
    )
}
