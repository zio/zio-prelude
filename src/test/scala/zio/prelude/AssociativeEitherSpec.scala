package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeEitherSpec extends DefaultRunnableSpec {

  def spec =
    suite("AssociativeEitherSpec")(
      suite("laws")(
        testM("option")(checkAllLaws(AssociativeEither)(GenF.option, Gen.anyInt)),
        testM("either")(checkAllLaws(AssociativeEither)(GenF.either(Gen.anyInt), Gen.anyInt))
      )
    )
}
