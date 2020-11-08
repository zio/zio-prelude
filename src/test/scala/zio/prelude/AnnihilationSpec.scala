package zio.prelude

import zio.test._
import zio.test.laws._

object AnnihilationSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilationSpec")(
      suite("laws")(
        testM("int annihilating zero")(checkAllLaws(Annihilation)(Gen.anyInt)),
        testM("double annihilating zero")(checkAllLaws(Annihilation)(Gen.anyDouble))
      )
    )
}
