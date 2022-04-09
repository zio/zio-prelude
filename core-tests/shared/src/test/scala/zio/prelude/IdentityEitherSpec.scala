package zio.prelude

import zio.prelude.laws.IdentityEitherlaws
import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Any] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityEitherlaws)(GenF.chunk, Gen.int)),
        test("list")(checkAllLaws(IdentityEitherlaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityEitherlaws)(GenF.option, Gen.int)),
        test("set")(checkAllLaws(IdentityEitherlaws)(GenF.set, Gen.int)),
        test("vector")(checkAllLaws(IdentityEitherlaws)(GenF.vector, Gen.int))
      )
    )
}
