package zio.prelude
package experimental

import zio.test._
import zio.test.laws._

object NoncontradictionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("NoncontradictionSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(Noncontradiction)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
