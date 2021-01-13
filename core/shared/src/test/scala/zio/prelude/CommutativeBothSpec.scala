package zio.prelude

import zio.test.laws._
import zio.test.{testM, _}

object CommutativeBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(CommutativeBoth)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        testM("list")(checkAllLaws(CommutativeBoth)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(CommutativeBoth)(GenF.option, Gen.anyInt)),
        testM("vector")(checkAllLaws(CommutativeBoth)(GenF.vector, Gen.vectorOf(Gen.anyInt)))
      )
    )
}
