package zio.prelude.experimental

import zio.test._
import zio.test.laws._

object AnnihilationSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilationSpec")(
      suite("laws")(
        testM("double annihilating")(checkAllLaws(Annihilation)(Gen.anyDouble)),
        testM("int annihilating")(checkAllLaws(Annihilation)(Gen.anyInt))
      )
    )
}
