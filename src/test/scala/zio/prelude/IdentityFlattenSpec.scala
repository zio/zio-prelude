package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityFlattenSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(IdentityFlatten)(GenF.option, Gen.anyInt))
    )
  )
}
