package zio.prelude

import scala.util.Try

import zio.prelude.Validation._
import zio.test.Assertion

/**
 * `Validation` represents either a successful value of type `A` or a
 * collection of one or more errors of type `E`. Unlike `Either`, `Validation`
 * does not "short circuit" on failures and instead allows accumulating
 * multiple errors. This can be particularly useful in validating data, where
 * we want to attempt to validate all of the data and retain information about
 * all errors that arose, rather than failing at the first error.
 */
sealed trait Validation[+E, +A] { self =>

  /**
   * A symbolic alias for `zipParLeft`.
   */
  final def <&[E1 >: E, B](that: Validation[E1, B]): Validation[E1, A] =
    zipParLeft(that)

  /**
   * A symbolic alias for `zipParRight`.
   */
  final def &>[E1 >: E, B](that: Validation[E1, B]): Validation[E1, B] =
    zipParRight(that)

  /**
   * A symbolic alias for `zipPar`.
   */
  final def <&>[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    zipPar(that)

  /**
   * Folds over the error and success values of this `Validation`.
   */
  final def fold[B](failure: (E, Vector[E]) => B, success: A => B): B =
    self match {
      case Failure(e, es) => failure(e, es)
      case Success(a)     => success(a)
    }

  /**
   * Transforms the successful value of this `Validation` with the specified
   * function.
   */
  final def map[B](f: A => B): Validation[E, B] =
    self match {
      case Success(a)              => Success(f(a))
      case failure @ Failure(_, _) => failure
    }

  /**
   * Transforms the error value of this `Validation` with the specified
   * function.
   */
  final def mapError[E1](f: E => E1): Validation[E1, A] =
    self match {
      case Failure(e, es)       => Failure(f(e), es.map(f))
      case success @ Success(_) => success
    }

  /**
   * Transforms this `Validation` to an `Either`.
   */
  final def toEither[E1 >: E]: Either[::[E1], A] =
    fold((e, es) => Left(::(e, es.toList)), a => Right(a))

  /**
   * Transforms this `Validation` to an `Option`, discarding information about
   * the errors.
   */
  final def toOption: Option[A] =
    fold((_, _) => None, a => Some(a))

  /**
   * A variant of `zipPar` that keeps only the left success value, but returns
   * a failure with all errors if either this `Validation` or the specififed
   * `Validation` fail.
   */
  final def zipParLeft[E1 >: E, B](that: Validation[E1, B]): Validation[E1, A] =
    zipWithPar(that)((a, _) => a)

  /**
   * A variant of `zipPar` that keeps only the right success value, but returns
   * a failure with all errors if either this `Validation` or the specififed
   * `Validation` fail.
   */
  final def zipParRight[E1 >: E, B](that: Validation[E1, B]): Validation[E1, B] =
    zipWithPar(that)((_, b) => b)

  /**
   * Combines this `Validation` with the specified `Validation`, returning a
   * tuple of their results. Returns either the combined result if both were
   * successes or otherwise returns a failure with all errors.
   */
  final def zipPar[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    zipWithPar(that)((_, _))

  /**
   * Combines this `Validation` with the specified `Validation`, using the
   * function `f` to combine their success values. Returns either the combined
   * result if both were successes or otherwise returns a failure with all
   * errors.
   */
  final def zipWithPar[E1 >: E, B, C](that: Validation[E1, B])(f: (A, B) => C): Validation[E1, C] =
    (self, that) match {
      case (Failure(e, es), Failure(e1, e1s)) => Failure(e, es ++ Vector(e1) ++ e1s)
      case (failure @ Failure(_, _), _)       => failure
      case (_, failure @ Failure(_, _))       => failure
      case (Success(a), Success(b))           => Success(f(a, b))
    }
}

object Validation {

  final case class Failure[+E](error: E, errors: Vector[E]) extends Validation[E, Nothing]
  final case class Success[+A](value: A)                    extends Validation[Nothing, A]

  /**
   * Attempts to evaluate the specified value, catching any error that occurs
   * during evaluation and capturing it as a failure.
   */
  def apply[A](a: => A): Validation[Throwable, A] =
    try {
      succeed(a)
    } catch {
      case e: VirtualMachineError => throw e
      case e: Throwable           => fail(e)
    }

  /**
   * Combine a collection of `Validation` values into a single `Validation`
   * that either returns the values of all of them, if their all succeed, or
   * else fails with all of their errors.
   */
  def collectAllPar[E, A](validations: Iterable[Validation[E, A]]): Validation[E, List[A]] =
    validations.foldRight[Validation[E, List[A]]](succeed(List.empty))(_.zipWithPar(_)(_ :: _))

  /**
   * Constructs a `Validation` that fails with the specified error.
   */
  def fail[E](error: E): Validation[E, Nothing] =
    Failure(error, Vector.empty)

  /**
   * Constructs a `Validation` from a value and an assertion about that value.
   * The resulting `Validation` will be a success if the value satisfies the
   * assertion or else will contain a string rendering describing how the
   * value did not satisfy the assertion.
   */
  def fromAssert[A](value: A)(assertion: Assertion[A]): Validation[String, A] =
    zio.Runtime.default.unsafeRun {
      assertion.test(value).map { b =>
        if (b)(succeed(value))
        else fail(s"$value did not satisfy ${assertion.render}")
      }
    }

  /**
   * Constructs a `Validation` from an `Either`.
   */
  def fromEither[E, A](value: Either[E, A]): Validation[E, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `Validation` from an `Option`.
   */
  def fromOption[A](value: Option[A]): Validation[Unit, A] =
    value.fold[Validation[Unit, A]](fail(()))(succeed)

  /**
   * Constructs a `Validation` from a `Try`.
   */
  def fromTry[A](value: => Try[A]): Validation[Throwable, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `Validation` that succeeds with the specified value.
   */
  def succeed[A](value: A): Validation[Nothing, A] =
    Success(value)

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A, B, C](a: Validation[E, A], b: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
    a.zipWithPar(b)(f)

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A, B, C, D](a: Validation[E, A], b: Validation[E, B], c: Validation[E, C])(
    f: (A, B, C) => D
  ): Validation[E, D] =
    (a <&> b <&> c).map { case ((a, b), c) => f(a, b, c) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A, B, C, D, F](a: Validation[E, A], b: Validation[E, B], c: Validation[E, C], d: Validation[E, D])(
    f: (A, B, C, D) => F
  ): Validation[E, F] =
    (a <&> b <&> c <&> d).map { case (((a, b), c), d) => f(a, b, c, d) }
}
