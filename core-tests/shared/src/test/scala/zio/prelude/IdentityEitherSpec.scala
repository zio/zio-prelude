package zio.prelude

import zio.prelude.laws.IdentityEitherlaws
import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityEitherlaws)(GenF.chunk, Gen.anyInt)),
        test("list")(checkAllLaws(IdentityEitherlaws)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityEitherlaws)(GenF.option, Gen.anyInt)),
        test("set")(checkAllLaws(IdentityEitherlaws)(GenF.set, Gen.anyInt)),
        test("vector")(checkAllLaws(IdentityEitherlaws)(GenF.vector, Gen.anyInt))
      )
    )
}
