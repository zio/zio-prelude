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
        testM("chunk . option")(checkAllLaws(AssociativeBoth)(chunkOptionGenF, Gen.anyInt))
      )
    )
}
