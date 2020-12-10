package zio.prelude

import zio.prelude.newtypes.Failure
import zio.random.Random
import zio.test.Gen.oneOf
import zio.test._
import zio.test.laws.GenF
import zio.{ Cause, Exit, NonEmptyChunk }

import scala.concurrent.Future
import scala.util.Try

/**
 * Provides higher kinded generators.
 */
object GenFs {

  /**
   * A generator of failed `Cause` values.
   */
  val cause: GenF[Random with Sized, Cause] =
    new GenF[Random with Sized, Cause] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, Cause[A]] =
        Gen.causes(gen, Gen.throwable)
    }

  def either[R <: Random with Sized, E](e: Gen[R, E]): GenF[R, ({ type lambda[+a] = Either[E, a] })#lambda] =
    new GenF[R, ({ type lambda[+a] = Either[E, a] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Either[E, A]] =
        Gen.either(e, a)
    }

  /**
   * A generator of `Exit` values.
   */
  def exit[R <: Random with Sized, E](e: Gen[R, Cause[E]]): GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] =
    new GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Exit[E, A]] =
        Gen.either(e, a).map {
          case Left(cause)    => Exit.halt(cause)
          case Right(success) => Exit.succeed(success)
        }
    }

  /**
   * A generator of `Future` values.
   */
  val future: GenF[Random with Sized, Future] =
    new GenF[Random with Sized, Future] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, Future[A]] =
        oneOf(Gen.throwable.map(Future.failed), gen.map(Future.successful))
    }

  def map[R <: Random with Sized, K](k: Gen[R, K]): GenF[R, ({ type lambda[+v] = Map[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = Map[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V]): Gen[R1, Map[K, V]] =
        Gen.mapOf(k, v)
    }

  /**
   * A generator of `NonEmptyChunk` values.
   */
  def nonEmptyChunk: GenF[Random with Sized, NonEmptyChunk] =
    new GenF[Random with Sized, NonEmptyChunk] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyChunk[A]] =
        Gen.chunkOf1(gen)
    }

  def nonEmptyList: GenF[Random with Sized, NonEmptyList] =
    new GenF[Random with Sized, NonEmptyList] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyList[A]] =
        Gens.nonEmptyListOf(gen)
    }

  def these[R <: Random with Sized, A](a: Gen[R, A]): GenF[R, ({ type lambda[+b] = These[A, b] })#lambda] =
    new GenF[R, ({ type lambda[+b] = These[A, b] })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B]): Gen[R1, These[A, B]] =
        Gens.these(a, b)
    }

  /**
   * A generator of `Try` values.
   */
  val tryScala: GenF[Random with Sized, Try] =
    new GenF[Random with Sized, Try] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, Try[A]] =
        oneOf(Gen.throwable.map(scala.util.Failure(_)), gen.map(scala.util.Success(_)))
    }

  def tuple2[R <: Random with Sized, A](a: Gen[R, A]): GenF[R, ({ type lambda[+x] = (A, x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = (A, x) })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B]): Gen[R1, (A, B)] =
        a.zip(b)
    }

  def tuple3[R <: Random with Sized, A, B](
    a: Gen[R, A],
    b: Gen[R, B]
  ): GenF[R, ({ type lambda[+c] = (A, B, c) })#lambda] =
    new GenF[R, ({ type lambda[+c] = (A, B, c) })#lambda] {
      def apply[R1 <: R, C](c: Gen[R1, C]): Gen[R1, (A, B, C)] =
        Gen.crossN(a, b, c)((a, b, c) => (a, b, c))
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
}
