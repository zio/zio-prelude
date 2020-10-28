package zio.prelude

import zio.test._
import zio.test.laws._

object InvolutionSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("InvolutionSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(Involution)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
