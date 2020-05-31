package zio.prelude

import zio.prelude.newtypes.Failure
import zio.{ Cause, Exit, NonEmptyChunk }
import zio.random.Random
import zio.test._
import zio.test.laws.GenF

/**
 * Provides higher kinded generators.
 */
object GenFs {

  def nonEmptyList: GenF[Random with Sized, NonEmptyList] =
    new GenF[Random with Sized, NonEmptyList] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyList[A]] =
        Gens.nonEmptyListOf(gen)
    }

  def tuple2[R <: Random with Sized, A](a: Gen[R, A]): GenF[R, ({ type lambda[+x] = (A, x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = (A, x) })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B]): Gen[R1, (A, B)] =
        a.zip(b)
    }

  def tuple3[R <: Random with Sized, A, B](a: Gen[R, (A, B)]): GenF[R, ({ type lambda[+x] = ((A, B), x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = ((A, B), x) })#lambda] {
      def apply[R1 <: R, C](b: Gen[R1, C]): Gen[R1, ((A, B), C)] =
        a.zip(b)
    }

  def validation[R <: Random with Sized, E](e: Gen[R, E]): GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Validation[E, A]] =
        Gens.validation(e, a)
    }

  def validationFailure[R <: Random with Sized, A](
    a: Gen[R, A]
  ): GenF[R, ({ type lambda[+x] = Failure[Validation[x, A]] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Failure[Validation[x, A]] })#lambda] {
      def apply[R1 <: R, E](e: Gen[R1, E]): Gen[R1, Failure[Validation[E, A]]] =
        Gens.validation(e, a).map(Failure.wrap)
    }

  /**
   * A generator of failed `Cause` values.
   */
  val fail: GenF[Random, Cause] =
    new GenF[Random, Cause] {
      def apply[R1 <: Random, A](gen: Gen[R1, A]): Gen[R1, Cause[A]] =
        gen.map(Cause.fail)
    }

  /**
   * A generator of `Exit` values.
   */
  def exit[R <: Random with Sized, E](e: Gen[R, E]): GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] =
    new GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Exit[E, A]] =
        Gen.either(e, a).map {
          case Left(error)    => Exit.fail(error)
          case Right(success) => Exit.succeed(success)
        }
    }

  /**
   * A generator of `NonEmptyChunk` values.
   */
  def nonEmptyChunk: GenF[Random with Sized, NonEmptyChunk] =
    new GenF[Random with Sized, NonEmptyChunk] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyChunk[A]] =
        Gen.chunkOf1(gen)
    }
}
