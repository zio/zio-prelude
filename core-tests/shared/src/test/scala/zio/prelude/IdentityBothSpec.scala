package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityBothSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityBothSpec")(
      suite("laws")(
        test("either")(checkAllLaws(IdentityBothLaws)(GenF.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityBothLaws)(GenF.option, Gen.int)),
        test("try")(checkAllLaws(IdentityBothLaws)(GenFs.tryScala, Gen.int))
      )
    )
}
