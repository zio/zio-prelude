package zio.prelude

import zio.test._
import zio.test.laws._

object AnnihilatingZeroSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilatingZeroSpec")(
      suite("laws")(
        testM("int annihilating zero")(checkAllLaws(AnnihilatingZero)(Gen.anyInt)),
        testM("double annihilating zero")(checkAllLaws(AnnihilatingZero)(Gen.anyDouble))
      )
    )
}
