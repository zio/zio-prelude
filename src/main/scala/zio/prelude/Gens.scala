package zio.prelude

import zio.{ Chunk, NonEmptyChunk }
import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

  /**
   * A generator of chunks.
   */
  def chunkOf[R <: Random with Sized, A](gen: Gen[R, A]): Gen[R, Chunk[A]] =
    Gen.listOf(gen).map(Chunk.fromIterable)

  /**
   * A generator of non-empty chunks.
   */
  def nonEmptyChunkOf[R <: Random with Sized, A](gen: Gen[R, A]): Gen[R, NonEmptyChunk[A]] =
    for {
      head <- gen
      tail <- Gen.sized(n => Gen.listOfN(n - 1 max 0)(gen))
    } yield Chunk(head) ++ Chunk.fromIterable(tail)

  /**
   * A generator of validation values.
   */
  def validation[R <: Random with Sized, E, A](e: Gen[R, E], a: Gen[R, A]): Gen[R, Validation[E, A]] =
    Gen.either(nonEmptyChunkOf(e), a).map {
      case Left(es) => Validation.Failure(es)
      case Right(a) => Validation.Success(a)
    }
}
