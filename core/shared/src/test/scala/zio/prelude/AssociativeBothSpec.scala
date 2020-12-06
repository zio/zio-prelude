package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeBothSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(AssociativeBoth)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        testM("either")(checkAllLaws(AssociativeBoth)(GenF.either(Gen.anyInt), Gen.anyInt)),
        testM("option")(checkAllLaws(AssociativeBoth)(GenF.option, Gen.anyInt)),
        testM("list")(checkAllLaws(AssociativeBoth)(GenF.list, Gen.anyInt)),
        testM("vector")(checkAllLaws(AssociativeBoth)(GenF.vector, Gen.vectorOf(Gen.anyInt)))
      )
    )
}
