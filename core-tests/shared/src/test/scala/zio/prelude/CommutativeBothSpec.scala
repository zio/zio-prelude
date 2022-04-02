package zio.prelude

import zio.prelude.laws.CommutativeBothLaws
import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(CommutativeBothLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("list")(checkAllLaws(CommutativeBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(CommutativeBothLaws)(GenF.option, Gen.int)),
        test("vector")(checkAllLaws(CommutativeBothLaws)(GenF.vector, Gen.vectorOf(Gen.int)))
      )
    )
}
