package zio.prelude

import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

  /**
   * A generator of validation values.
   */
  def validation[R <: Random with Sized, E, A](e: Gen[R, E], a: Gen[R, A]): Gen[R, Validation[E, A]] =
    Gen.either(Gen.chunkOf1(e), a).map {
      case Left(es) => Validation.Failure(es)
      case Right(a) => Validation.Success(a)
    }
}
