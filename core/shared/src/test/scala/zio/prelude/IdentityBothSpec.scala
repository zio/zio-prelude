package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityBothSpec")(
      suite("laws")(
        test("either")(checkAllLaws(IdentityBoth)(GenF.either(Gen.anyInt), Gen.anyInt)),
        test("list")(checkAllLaws(IdentityBoth)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityBoth)(GenF.option, Gen.anyInt)),
        test("try")(checkAllLaws(IdentityBoth)(GenFs.tryScala, Gen.anyInt))
      )
    )
}
