package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityBothSpec")(
      suite("laws")(
        test("either")(checkAllLaws(IdentityBothLaws)(GenF.either(Gen.anyInt), Gen.anyInt)),
        test("list")(checkAllLaws(IdentityBothLaws)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityBothLaws)(GenF.option, Gen.anyInt)),
        test("try")(checkAllLaws(IdentityBothLaws)(GenFs.tryScala, Gen.anyInt))
      )
    )
}
