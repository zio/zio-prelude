package zio.prelude
package experimental

import zio.test._
import zio.test.laws._

object AbsorptionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AbsorptionSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(Absorption)(Gen.setOf(Gen.anyInt))),
        testM("boolean")(checkAllLaws(Absorption)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
