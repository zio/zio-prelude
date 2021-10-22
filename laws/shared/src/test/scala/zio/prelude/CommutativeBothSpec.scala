package zio.prelude

import zio.prelude.laws.CommutativeBothLaws
import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(CommutativeBothLaws)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        testM("list")(checkAllLaws(CommutativeBothLaws)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(CommutativeBothLaws)(GenF.option, Gen.anyInt)),
        testM("vector")(checkAllLaws(CommutativeBothLaws)(GenF.vector, Gen.vectorOf(Gen.anyInt)))
      )
    )
}
