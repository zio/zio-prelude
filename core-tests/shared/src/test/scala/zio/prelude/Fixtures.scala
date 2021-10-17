package zio.prelude

import zio.test.laws.GenF
import zio.test.{Gen, Sized}
import zio.{Chunk, Has, Random, ZTraceElement}

object Fixtures {
  type ChunkOption[+A] = Chunk[Option[A]]

  val chunkOptionGenF: GenF[Has[Random] with Has[Sized], ChunkOption] =
    new GenF[Has[Random] with Has[Sized], ChunkOption] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
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
