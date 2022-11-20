package zio.prelude.experimental

import zio.prelude._
import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object AnnihilationSpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("AnnihilationSpec")(
      suite("laws")(
        testM("double annihilating")(checkAllLaws(AnnihilationLaws)(Gen.anyDouble)),
        testM("int annihilating")(checkAllLaws(AnnihilationLaws)(Gen.anyInt))
      )
    )
}
