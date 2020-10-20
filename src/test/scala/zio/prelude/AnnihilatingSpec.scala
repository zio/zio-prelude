package zio.prelude

import zio.test.laws._
import zio.test.{ DefaultRunnableSpec, _ }

object AnnihilatingSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilatingSpec")(
      suite("laws")(
        testM("int annihilating")(checkAllLaws(Annihilating)(Gen.anyInt))
      )
    )
}
