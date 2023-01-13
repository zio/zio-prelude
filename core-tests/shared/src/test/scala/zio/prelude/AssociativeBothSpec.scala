package zio.prelude

import zio.Chunk
import zio.prelude.laws.AssociativeBothLaws
import zio.test._
import zio.test.laws._

object AssociativeBothSpec extends ZIOSpecDefault {
  import Fixtures._

  implicit val chunkOptionAssociativeBoth: AssociativeBoth[ChunkOption] =
    AssociativeBoth.compose[Chunk, Option]

  def spec: Spec[Environment, Any] =
    suite("AssociativeBothSpec")(
      suite("laws")(
        test("chunk . option")(checkAllLaws(AssociativeBothLaws)(chunkOptionGenF, Gen.int))
      ),
      test("associativeBoth can be derived from Covariant and AssociativeFlatten") {
        trait F[+A]
        implicit def covariant: Covariant[F]                   = ???
        implicit def associativeFlatten: AssociativeFlatten[F] = ???
        implicitly[AssociativeBoth[F]]
        assertCompletes
      }
    )
}
