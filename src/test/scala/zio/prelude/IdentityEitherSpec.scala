package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityEitherFSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityEitherFSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityEitherF)(GenFs.option, Gen.anyInt))
    )
  )
}
