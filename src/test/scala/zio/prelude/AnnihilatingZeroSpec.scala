package zio.prelude

import zio.test._
import zio.test.laws._

object AnnihilatingZeroSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilatingZeroSpec")(
      suite("laws")(
        testM("int annihilating")(checkAllLaws(AnnihilatingZero)(Gen.anyInt)),
        testM("double distributive")(checkAllLaws(AnnihilatingZero)(Gen.anyDouble))
      )
    )
}
