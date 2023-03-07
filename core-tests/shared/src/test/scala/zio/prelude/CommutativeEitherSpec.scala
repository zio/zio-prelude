package zio.prelude

import zio.prelude.laws.CommutativeEitherLaws
import zio.test._
import zio.test.laws._

object CommutativeEitherSpec extends ZIOBaseSpec {

  IdentityEither[Set]
  CommutativeEither[Set]

  coherent.CommutativeEitherDeriveEqualInvariant.derive[Set]

  def spec: Spec[Environment, Any] =
    suite("CommutativeEitherSpec")(
      suite("laws")(
        test("set")(checkAllLaws(CommutativeEitherLaws)(GenF.set, Gen.int))
      )
    )
}
