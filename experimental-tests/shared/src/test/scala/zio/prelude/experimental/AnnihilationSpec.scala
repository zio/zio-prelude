package zio.prelude.experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object AnnihilationSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilationSpec")(
      suite("laws")(
        testM("double annihilating")(checkAllLaws(AnnihilationLaws)(Gen.anyDouble)),
        testM("int annihilating")(checkAllLaws(AnnihilationLaws)(Gen.anyInt))
      )
    )
}
