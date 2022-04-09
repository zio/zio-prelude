package zio.prelude

import zio.prelude.laws.CommutativeEitherLaws
import zio.test._
import zio.test.laws._

object CommutativeEitherSpec extends ZIOSpecDefault {

  IdentityEither[Set]
  CommutativeEither[Set]

  coherent.CommutativeEitherDeriveEqualInvariant.derive[Set]

  def spec: ZSpec[Environment, Any] =
    suite("CommutativeEitherSpec")(
      suite("laws")(
        test("set")(checkAllLaws(CommutativeEitherLaws)(GenF.set, Gen.int))
      )
    )
}
