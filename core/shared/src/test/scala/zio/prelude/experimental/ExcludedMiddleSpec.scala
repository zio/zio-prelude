package zio.prelude
package experimental

import zio.test._
import zio.test.laws._

object ExcludedMiddleSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ExcludedMiddleSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(ExcludedMiddle)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
