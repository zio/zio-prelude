package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityBothSpec")(
      suite("laws")(
        testM("either")(checkAllLaws(IdentityBothLaws)(GenF.either(Gen.anyInt), Gen.anyInt)),
        testM("list")(checkAllLaws(IdentityBothLaws)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(IdentityBothLaws)(GenF.option, Gen.anyInt)),
        testM("try")(checkAllLaws(IdentityBothLaws)(GenFs.tryScala, Gen.anyInt))
      )
    )
}
