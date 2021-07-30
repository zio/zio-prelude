package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityEither)(GenF.chunk, Gen.anyInt)),
        test("list")(checkAllLaws(IdentityEither)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityEither)(GenF.option, Gen.anyInt)),
        test("set")(checkAllLaws(IdentityEither)(GenF.set, Gen.anyInt)),
        test("vector")(checkAllLaws(IdentityEither)(GenF.vector, Gen.anyInt))
      )
    )
}
