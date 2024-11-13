package zio.prelude

import zio.prelude.laws.CommutativeBothLaws
import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends ZIOBaseSpec {
  import Fixtures._

  def spec: Spec[Environment, Any] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(CommutativeBothLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("list")(checkAllLaws(CommutativeBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(CommutativeBothLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(CommutativeBothLaws)(optionalGenF, Gen.int)),
        test("vector")(checkAllLaws(CommutativeBothLaws)(GenF.vector, Gen.vectorOf(Gen.int)))
      )
    )
}
