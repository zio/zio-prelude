package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityBothSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityBoth)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(IdentityBoth)(GenF.list, Gen.anyInt))
    )
  )
}
