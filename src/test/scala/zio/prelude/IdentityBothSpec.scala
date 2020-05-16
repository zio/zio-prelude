package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityBothSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityBoth)(GenFs.option, Gen.anyInt))
    )
  )
}
