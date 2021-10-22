package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object AbsorptionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AbsorptionSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(AbsorptionLaws)(Gen.setOf(Gen.anyInt))),
        testM("boolean")(checkAllLaws(AbsorptionLaws)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
