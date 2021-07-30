package zio.prelude

import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(CommutativeBoth)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        test("list")(checkAllLaws(CommutativeBoth)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(CommutativeBoth)(GenF.option, Gen.anyInt)),
        test("vector")(checkAllLaws(CommutativeBoth)(GenF.vector, Gen.vectorOf(Gen.anyInt)))
      )
    )
}
