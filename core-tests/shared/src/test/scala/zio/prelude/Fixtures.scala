package zio.prelude

import zio.Chunk
import zio.random.Random
import zio.test.laws.GenF
import zio.test.{Gen, Sized}

object Fixtures {
  type ChunkOption[+A] = Chunk[Option[A]]

  val chunkOptionGenF: GenF[Random with Sized, ChunkOption] =
    new GenF[Random with Sized, ChunkOption] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]) =
        Gen.chunkOf(Gen.option(gen))
    }

  implicit val chunkOptionDeriveEqual: DeriveEqual[ChunkOption] =
    new DeriveEqual[ChunkOption] {
      def derive[A: Equal] = Equal[ChunkOption[A]]
    }

  implicit val chunkOptionInvariant: Invariant[ChunkOption] =
    Invariant[Chunk].compose[Option]
}
