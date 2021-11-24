package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object ExcludedMiddleSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ExcludedMiddleSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(ExcludedMiddleLaws)(Gen.boolean))
      )
    )
}
