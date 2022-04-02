package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object DistributiveAbsorptionSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveAbsorptionSpec")(
      suite("laws")(
        test("set")(checkAllLaws(DistributiveAbsorptionLaws)(Gen.setOf(Gen.int))),
        test("boolean")(checkAllLaws(DistributiveAbsorptionLaws)(Gen.boolean))
      )
    )
}
