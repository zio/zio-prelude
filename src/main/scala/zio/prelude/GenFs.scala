package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws.GenF

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object GenFs {

  def nonEmptyList: GenF[Random with Sized, NonEmptyList] =
    new GenF[Random with Sized, NonEmptyList] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyList[A]] =
        Gens.nonEmptyListOf(gen)
    }

  /**
   * A generator of validation values.
   */
  def validation[R <: Random with Sized, E](e: Gen[R, E]): GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Validation[E, A]] =
        Gens.validation(e, a)
    }
}
