package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityBothFSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityBothFSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityBothF)(GenFs.option, Gen.anyInt))
    )
  )
}
