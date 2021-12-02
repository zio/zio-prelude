package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object DistributiveAbsorptionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveAbsorptionSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(DistributiveAbsorptionLaws)(Gen.setOf(Gen.anyInt))),
        testM("boolean")(checkAllLaws(DistributiveAbsorptionLaws)(Gen.boolean))
      )
    )
}
