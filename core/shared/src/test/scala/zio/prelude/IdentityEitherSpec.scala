package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(IdentityEither)(GenF.chunk, Gen.anyInt)),
        testM("list")(checkAllLaws(IdentityEither)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(IdentityEither)(GenF.option, Gen.anyInt)),
        testM("set")(checkAllLaws(IdentityEither)(GenF.set, Gen.anyInt)),
        testM("vector")(checkAllLaws(IdentityEither)(GenF.vector, Gen.anyInt))
      )
    )
}
