package zio.prelude

import zio.prelude.data.Optional
import zio.test.laws.GenF
import zio.test.{Gen, Sized}
import zio.{Chunk, Trace}

object Fixtures {
  type ChunkOption[+A] = Chunk[Option[A]]

  val chunkOptionGenF: GenF[Sized, ChunkOption] =
    new GenF[Sized, ChunkOption] {
      def apply[R1 <: Sized, A](gen: Gen[R1, A])(implicit
        trace: Trace
      ): Gen[R1, Chunk[Option[A]]] =
        Gen.chunkOf(Gen.option(gen))
    }

  implicit val chunkOptionDeriveEqual: DeriveEqual[ChunkOption] =
    new DeriveEqual[ChunkOption] {
      def derive[A: Equal]: Equal[ChunkOption[A]] = Equal[ChunkOption[A]]
    }

  implicit val chunkOptionInvariant: Invariant[ChunkOption] =
    Invariant[Chunk].compose[Option]

  def anyOptional[R, A](gen: Gen[R, A])(implicit trace: Trace): Gen[R, Optional[A]] =
    Gen.option(gen).map(Optional.OptionIsNullable)

  val optionalGenF: GenF[Any, Optional] =
    new GenF[Any, Optional] {
      def apply[R1, A](gen: Gen[R1, A])(implicit trace: Trace): Gen[R1, Optional[A]] =
        anyOptional(gen)
    }

}
