package zio.prelude

import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

  /**
   * A generator of `NonEmptyList` values.
   */
  def nonEmptyListOf[R <: Random with Sized, A](a: Gen[R, A]): Gen[R, NonEmptyList[A]] =
    Gen.listOf1(a).map(NonEmptyList.fromCons)

  /**
   * A generator of state transition functions.
   */
  def state[R, S, A](s: Gen[R, S], a: Gen[R, A]): Gen[R, State[S, A]] =
    Gen.function[R, S, (S, A)](s <*> a).map(State(_))

  /**
   * A generator of `Validation` values.
   */
  def validation[R <: Random with Sized, E, A](e: Gen[R, E], a: Gen[R, A]): Gen[R, Validation[E, A]] =
    Gen.either(Gen.chunkOf1(e), a).map {
      case Left(es) => Validation.Failure(es)
      case Right(a) => Validation.Success(a)
    }
}
