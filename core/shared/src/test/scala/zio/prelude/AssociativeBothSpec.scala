package zio.prelude

import zio.Chunk
import zio.test._
import zio.test.laws._

object AssociativeBothSpec extends DefaultRunnableSpec {
  import Fixtures._

  implicit val chunkOptionAssociativeBoth: AssociativeBoth[ChunkOption] =
    AssociativeBoth.compose[Chunk, Option]

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeBothSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(AssociativeBoth)(GenF.chunk, Gen.anyInt)),
        testM("either")(checkAllLaws(AssociativeBoth)(GenF.either(Gen.anyInt), Gen.anyInt)),
        testM("option")(checkAllLaws(AssociativeBoth)(GenF.option, Gen.anyInt)),
        testM("list")(checkAllLaws(AssociativeBoth)(GenF.list, Gen.anyInt)),
        testM("vector")(checkAllLaws(AssociativeBoth)(GenF.vector, Gen.anyInt)),
        testM("chunk . option")(checkAllLaws(AssociativeBoth)(chunkOptionGenF, Gen.anyInt))
      )
    )
}
