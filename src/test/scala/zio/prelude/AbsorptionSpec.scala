package zio.prelude

import zio.test._
import zio.test.laws._

object AbsorptionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AbsorptionSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(Absorption)(Gen.setOf(Gen.anyInt)))
      )
    )
}
