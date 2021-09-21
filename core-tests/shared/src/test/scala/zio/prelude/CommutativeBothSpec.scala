package zio.prelude

import zio.prelude.laws.CommutativeBothLaws
import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(CommutativeBothLaws)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        test("list")(checkAllLaws(CommutativeBothLaws)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(CommutativeBothLaws)(GenF.option, Gen.anyInt)),
        test("vector")(checkAllLaws(CommutativeBothLaws)(GenF.vector, Gen.vectorOf(Gen.anyInt)))
      )
    )
}
