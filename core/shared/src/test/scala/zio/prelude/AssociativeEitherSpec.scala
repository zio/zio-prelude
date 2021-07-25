package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeEitherSpec")(
      suite("laws")(
        testM("either")(checkAllLaws(AssociativeEither)(GenF.either(Gen.anyInt), Gen.anyInt))
      )
    )
}
