package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object InvolutionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("InvolutionSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(InvolutionLaws)(Gen.boolean))
      )
    )
}
