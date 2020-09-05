package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("IdentityEitherSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityEither)(GenF.option, Gen.anyInt))
    )
  )
}
