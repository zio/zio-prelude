package zio.prelude

import zio.test.laws.GenF
import zio.test.{Gen, Sized}
import zio.{Chunk, Random, ZTraceElement}

object Fixtures {
  type ChunkOption[+A] = Chunk[Option[A]]

  val chunkOptionGenF: GenF[Random with Sized, ChunkOption] =
    new GenF[Random with Sized, ChunkOption] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, Chunk[Option[A]]] =
        Gen.chunkOf(Gen.option(gen))
    }

  implicit val chunkOptionDeriveEqual: DeriveEqual[ChunkOption] =
    new DeriveEqual[ChunkOption] {
      def derive[A: Equal]: Equal[ChunkOption[A]] = Equal[ChunkOption[A]]
    }

  implicit val chunkOptionInvariant: Invariant[ChunkOption] =
    Invariant[Chunk].compose[Option]
}
