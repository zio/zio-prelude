package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityBothSpec extends ZIOBaseSpec {
  import Fixtures._

  def spec: Spec[Environment, Any] =
    suite("IdentityBothSpec")(
      suite("laws")(
        test("either")(checkAllLaws(IdentityBothLaws)(GenF.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityBothLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(IdentityBothLaws)(optionalGenF, Gen.int)),
        test("try")(checkAllLaws(IdentityBothLaws)(GenFs.tryScala, Gen.int))
      )
    )
}
