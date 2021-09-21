package zio.prelude

import zio.prelude.laws.IdentityEitherlaws
import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(IdentityEitherlaws)(GenF.chunk, Gen.anyInt)),
        testM("list")(checkAllLaws(IdentityEitherlaws)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(IdentityEitherlaws)(GenF.option, Gen.anyInt)),
        testM("set")(checkAllLaws(IdentityEitherlaws)(GenF.set, Gen.anyInt)),
        testM("vector")(checkAllLaws(IdentityEitherlaws)(GenF.vector, Gen.anyInt))
      )
    )
}
