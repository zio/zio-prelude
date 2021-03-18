package zio.prelude

import zio.prelude.ZValidation._
import zio.test.Assertion
import zio.{Chunk, IO, NonEmptyChunk, ZIO}

import scala.util.Try

/**
 * `ZValidation` represents either a success of type `A` or a collection of one
 * or more errors of type `E` along with in either case a log with entries of
 * type `W`. Unlike `Either`, `ZValidation` does not "short circuit" on
 * failures and instead allows accumulating multiple errors. This can be
 * particularly useful in validating data, where we want to attempt to validate
 * all of the data and retain information about all errors that arose, rather
 * than failing at the first error.
 */
sealed trait ZValidation[+W, +E, +A] { self =>

  /**
   * A symbolic alias for `zipParLeft`.
   */
  final def <&[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, A] =
    zipParLeft(that)

  /**
   * A symbolic alias for `zipParRight`.
   */
  final def &>[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, B] =
    zipParRight(that)

  /**
   * A symbolic alias for `zipPar`.
   */
  final def <&>[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, (A, B)] =
    zipPar(that)

  /**
   * A symbolic alias for `log`.
   */
  final def ??[W1 >: W](w1: W1): ZValidation[W1, E, A] =
    log(w1)

  /**
   * Returns whether this `ZValidation` and the specified `ZValidation` are
   * equal to each other.
   */
  override final def equals(that: Any): Boolean =
    (self, that) match {
      case (Failure(_, e), Failure(_, e1)) => e == e1
      case (Success(_, a), Success(_, a1)) => a == a1
      case _                               => false
    }

  /**
   * Transforms the value of this `ZValidation` with the specified validation
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def flatMap[W1 >: W, E1 >: E, B](f: A => ZValidation[W1, E1, B]): ZValidation[W1, E1, B] =
    self match {
      case Failure(w, e) => Failure(w, e)
      case Success(w, a) =>
        f(a) match {
          case Failure(w1, e) => Failure(w ++ w1, e)
          case Success(w1, b) => Success(w ++ w1, b)
        }
    }

  /**
   * Transforms the value of this `ZValidation` with the specified effectual
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def forEach[F[+_]: IdentityBoth: Covariant, B](f: A => F[B]): F[ZValidation[W, E, B]] =
    self match {
      case Failure(w, e) => Failure(w, e).succeed[F]
      case Success(w, a) => f(a).map(Success(w, _))
    }

  /**
   * Folds over the error and success values of this `ZValidation`.
   */
  final def fold[B](failure: NonEmptyChunk[E] => B, success: A => B): B =
    self match {
      case Failure(_, e) => failure(e)
      case Success(_, a) => success(a)
    }

  /**
   * Returns the value of the log.
   */
  final def getLog: Chunk[W] =
    self match {
      case Failure(w, _) => w
      case Success(w, _) => w
    }

  /**
   * Writes an entry to the log.
   */
  final def log[W1 >: W](w1: W1): ZValidation[W1, E, A] =
    self match {
      case Failure(w, e) => Failure(w :+ w1, e)
      case Success(w, a) => Success(w :+ w1, a)
    }

  /**
   * Transforms the successful value of this `ZValidation` with the specified
   * function.
   */
  final def map[B](f: A => B): ZValidation[W, E, B] =
    self match {
      case Failure(w, e) => Failure(w, e)
      case Success(w, a) => Success(w, f(a))
    }

  /**
   * Transforms the error value of this `ZValidation` with the specified
   * function.
   */
  final def mapError[E2](f: E => E2): ZValidation[W, E2, A] =
    self match {
      case Failure(w, e) => Failure(w, e.map(f))
      case Success(w, a) => Success(w, a)
    }

  /**
   * Transforms all the error values of this `ZValidation` with the specified
   * function.
   */
  final def mapErrorAll[E2](f: NonEmptyChunk[E] => NonEmptyChunk[E2]): ZValidation[W, E2, A] =
    self match {
      case Failure(w, e) => Failure(w, f(e))
      case Success(w, a) => Success(w, a)
    }

  /**
   * Transforms the log entries of this `ZValidation` with the specified
   * function.
   */
  final def mapLog[W2](f: W => W2): ZValidation[W2, E, A] =
    self match {
      case Failure(w, e) => Failure(w.map(f), e)
      case Success(w, a) => Success(w.map(f), a)
    }

  /**
   * Transforms all the log entries of this `ZValidation` with the specified
   * function.
   */
  final def mapLogAll[W2](f: Chunk[W] => Chunk[W2]): ZValidation[W2, E, A] =
    self match {
      case Failure(w, e) => Failure(f(w), e)
      case Success(w, a) => Success(f(w), a)
    }

  /**
   * Exposes the result of this validation function as either a `Right` with
   * a success of type `A` or a `Left` with one or more errors of type `E`,
   * along with the log.
   */
  final def runLog[B]: (Chunk[W], Either[NonEmptyChunk[E], A]) =
    self match {
      case Failure(w, e) => (w, Left(e))
      case Success(w, a) => (w, Right(a))
    }

  /**
   * Transforms this `ZValidation` to an `Either`, discarding the log.
   */
  final def toEither[E1 >: E]: Either[NonEmptyChunk[E1], A] =
    fold(Left(_), Right(_))

  /**
   * Transforms this `ZValidation` to an `Either`, transforming the accumulated errors and discarding the log.
   */
  final def toEitherWith[E2](f: NonEmptyChunk[E] => E2): Either[E2, A] =
    toEither.left.map(f)

  /**
   * Transforms this `ZValidation` to an `Option`, discarding information about
   * the errors and log.
   */
  final def toOption: Option[A] =
    fold(_ => None, Some(_))

  /**
   * Transforms this `ZValidation` to a `Try`, discarding all but the first
   * error and the log.
   */
  final def toTry(implicit ev: E <:< Throwable): scala.util.Try[A] =
    fold(es => scala.util.Failure(ev(es.head)), scala.util.Success(_))

  /**
   * Converts this `ZValidation` into a `ZIO` effect, discarding the log.
   */
  final def toZIO: IO[E, A] =
    self.fold(
      nec => ZIO.halt(nec.reduceMapLeft(zio.Cause.fail)((c, e) => zio.Cause.Both(c, zio.Cause.fail(e)))),
      ZIO.succeedNow
    )

  /**
   * A variant of `zipPar` that keeps only the left success value, but returns
   * a failure with all errors if either this `ZValidation` or the specified
   * `ZValidation` fail.
   */
  final def zipParLeft[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, A] =
    zipWithPar(that)((a, _) => a)

  /**
   * A variant of `zipPar` that keeps only the right success value, but returns
   * a failure with all errors if either this `ZValidation` or the specified
   * `ZValidation` fail.
   */
  final def zipParRight[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, B] =
    zipWithPar(that)((_, b) => b)

  /**
   * Combines this `ZValidation` with the specified `ZValidation`, returning a
   * tuple of their results. Returns either the combined result if both were
   * successes or otherwise returns a failure with all errors.
   */
  final def zipPar[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B]): ZValidation[W1, E1, (A, B)] =
    zipWithPar(that)((_, _))

  /**
   * Combines this `ZValidation` with the specified `ZValidation`, using the
   * function `f` to combine their success values. Returns either the combined
   * result if both were successes or otherwise returns a failure with all
   * errors.
   */
  final def zipWithPar[W1 >: W, E1 >: E, B, C](that: ZValidation[W1, E1, B])(f: (A, B) => C): ZValidation[W1, E1, C] =
    (self, that) match {
      case (Failure(w, e), Failure(w1, e1)) => Failure(w ++ w1, e ++ e1)
      case (Failure(w, e), Success(w1, _))  => Failure(w ++ w1, e)
      case (Success(w, _), Failure(w1, e1)) => Failure(w ++ w1, e1)
      case (Success(w, a), Success(w1, b))  => Success(w ++ w1, f(a, b))
    }
}

object ZValidation extends LowPriorityValidationImplicits {

  final case class Failure[+W, +E](log: Chunk[W], errors: NonEmptyChunk[E]) extends ZValidation[W, E, Nothing]
  final case class Success[+W, +A](log: Chunk[W], value: A)                 extends ZValidation[W, Nothing, A]

  /**
   * The `Covariant` instance for `ZValidation`.
   */
  implicit def ZValidationCovariant[W, E]: Covariant[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      def map[A, B](f: A => B): ZValidation[W, E, A] => ZValidation[W, E, B] =
        _.map(f)
    }

  /**
   * Derives a `Debug[ZValidation[W, E, A]]` given a `Debug[W], a `Debug[E]`,
   * and a `Debug[A]`.
   */
  implicit def ZValidationDebug[W: Debug, E: Debug, A: Debug]: Debug[ZValidation[W, E, A]] = {
    case Failure(w, e) => Debug.Repr.VConstructor(List("zio", "prelude"), "ZValidation.Failure", List(w.debug, e.debug))
    case Success(w, a) => Debug.Repr.VConstructor(List("zio", "prelude"), "ZValidation.Success", List(w.debug, a.debug))
  }

  /**
   * Derives an `Equal[ZValidation[W, E, A]]` given an `Equal[A]`.
   */
  implicit def ZValidationEqual[W, E, A: Equal]: Equal[ZValidation[W, E, A]] =
    Equal.make {
      case (Failure(_, e), Failure(_, e1)) => MultiSet.fromIterable(e) == MultiSet.fromIterable(e1)
      case (Success(_, a), Success(_, a1)) => a === a1
      case _                               => false
    }

  /**
   * The `DeriveEqual` instance for `ZValidation`.
   */
  implicit def ZValidationDeriveEqual[W, E: Equal]: DeriveEqual[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new DeriveEqual[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      def derive[A: Equal]: Equal[ZValidation[W, E, A]] =
        ZValidationEqual
    }

  /**
   * The `Covariant` instance for `ZValidation` with respect to its error type.
   */
  implicit def ZValidationFailureCovariant[W, A]
    : Covariant[({ type lambda[+e] = newtypes.Failure[ZValidation[W, e, A]] })#lambda] =
    new Covariant[({ type lambda[+e] = newtypes.Failure[ZValidation[W, e, A]] })#lambda] {
      def map[E, E2](f: E => E2): newtypes.Failure[ZValidation[W, E, A]] => newtypes.Failure[ZValidation[W, E2, A]] =
        validation => newtypes.Failure.wrap(newtypes.Failure.unwrap(validation).mapError(f))
    }

  /**
   * The `DeriveEqual` instance for `ZValidation` with respect to its error type.
   */
  implicit def ZValidationFailureDeriveEqual[W: Equal, A: Equal]
    : DeriveEqual[({ type lambda[+e] = newtypes.Failure[ZValidation[W, e, A]] })#lambda] =
    new DeriveEqual[({ type lambda[+e] = newtypes.Failure[ZValidation[W, e, A]] })#lambda] {
      def derive[E: Equal]: Equal[newtypes.Failure[ZValidation[W, E, A]]] =
        ZValidationEqual[W, E, A].contramap(newtypes.Failure.unwrap)
    }

  /**
   * The `ForEach` instance for `ZValidation`.
   */
  implicit def ZValidationForEach[W, E]: ForEach[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new ForEach[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      def forEach[F[+_]: IdentityBoth: Covariant, A, B](
        fa: ZValidation[W, E, A]
      )(f: A => F[B]): F[ZValidation[W, E, B]] =
        fa.forEach(f)
    }

  /**
   * The `IdentityBoth` instance for `ZValidation`.
   */
  implicit def ZValidationIdentityBoth[W, E]: IdentityBoth[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new IdentityBoth[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      val any: Validation[Nothing, Any]                                                                   =
        ZValidation.unit
      def both[A, B](fa: => ZValidation[W, E, A], fb: => ZValidation[W, E, B]): ZValidation[W, E, (A, B)] =
        fa.zipPar(fb)
    }

  /**
   * Derives a `PartialOrd[ZValidation[W, E, A]]` given an `Ord[E]` and an `Ord[A]`.
   */
  implicit def ZValidationPartialOrd[W, E: PartialOrd, A: PartialOrd]: PartialOrd[ZValidation[W, E, A]] =
    PartialOrd[MultiSet[E]].eitherWith(PartialOrd[A])(_.fold(e => Left(MultiSet.fromIterable(e)), a => Right(a)))

  /**
   * Attempts to evaluate the specified value, catching any error that occurs
   * during evaluation and capturing it as a failure.
   */
  def apply[A](a: => A): Validation[Throwable, A] =
    try succeed(a)
    catch {
      case e: VirtualMachineError => throw e
      case e: Throwable           => fail(e)
    }

  /**
   * Combine a collection of `ZValidation` values into a single `ZValidation`
   * that either returns the values of all of them, if their all succeed, or
   * else fails with all of their errors.
   */
  def collectAllPar[W, E, A](validations: Iterable[ZValidation[W, E, A]]): ZValidation[W, E, List[A]] =
    validations.foldRight[ZValidation[W, E, List[A]]](succeed(List.empty))(_.zipWithPar(_)(_ :: _))

  /**
   * Constructs a `ZValidation` that fails with the specified error.
   */
  def fail[E](error: E): Validation[E, Nothing] =
    Failure(Chunk.empty, NonEmptyChunk(error))

  /**
   * Constructs a `ZValidation` from a value and an assertion about that value.
   * The resulting `ZValidation` will be a success if the value satisfies the
   * assertion or else will contain a string rendering describing how the
   * value did not satisfy the assertion.
   */
  def fromAssert[A](value: A)(assertion: Assertion[A]): Validation[String, A] =
    if (assertion.test(value)) succeed(value)
    else fail(s"$value did not satisfy ${assertion.render}")

  /**
   * Constructs a `ZValidation` from an `Either`.
   */
  def fromEither[E, A](value: Either[E, A]): Validation[E, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `ZValidation` from an `Option`.
   */
  def fromOption[A](value: Option[A]): Validation[Unit, A] =
    value.fold[Validation[Unit, A]](fail(()))(succeed)

  /**
   * Constructs a `Validation` from a predicate, failing with None.
   */
  def fromPredicate[A](value: A)(f: A => Boolean): Validation[None.type, A] =
    fromPredicateWith(None)(value)(f)

  /**
   * Constructs a `Validation` from a predicate, failing with the error provided.
   */
  def fromPredicateWith[E, A](error: => E)(value: A)(f: A => Boolean): Validation[E, A] =
    if (f(value)) Validation.succeed(value)
    else Validation.fail(error)

  /**
   * Constructs a `ZValidation` from a `Try`.
   */
  def fromTry[A](value: => Try[A]): Validation[Throwable, A] =
    value match {
      case scala.util.Failure(e) => fail(e)
      case scala.util.Success(a) => succeed(a)
    }

  /**
   * Constructs a `ZValidation` that succeeds with the `Unit` value with a log
   * containing the specified entry.
   */
  def log[W](w: W): ZValidation[W, Nothing, Unit] =
    Success(Chunk(w), ())

  /**
   * Constructs a `Validation` that succeeds with the specified value.
   */
  def succeed[A](value: A): Validation[Nothing, A] =
    Success(Chunk.empty, value)

  /**
   * The `Validation` that succeeds with the `Unit` value.
   */
  val unit: Validation[Nothing, Unit] =
    succeed(())

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1](a0: ZValidation[W, E, A0], a1: ZValidation[W, E, A1]): ZValidation[W, E, (A0, A1)] =
    a0 <&> a1

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2]
  ): ZValidation[W, E, (A0, A1, A2)] =
    validateWith(a0, a1, a2)((_, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3]
  ): ZValidation[W, E, (A0, A1, A2, A3)] =
    validateWith(a0, a1, a2, a3)((_, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4)] =
    validateWith(a0, a1, a2, a3, a4)((_, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5)] =
    validateWith(a0, a1, a2, a3, a4, a5)((_, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6)((_, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7)((_, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)((_, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)((_, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)((_, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)((_, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19]
  ): ZValidation[W, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19],
    a20: ZValidation[W, E, A20]
  ): ZValidation[
    W,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
  ] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def validate[
    W,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21
  ](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19],
    a20: ZValidation[W, E, A20],
    a21: ZValidation[W, E, A21]
  ): ZValidation[
    W,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  ] =
    validateWith(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZValidation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, B](a0: ZValidation[W, E, A0], a1: ZValidation[W, E, A1])(
    f: (A0, A1) => B
  ): ZValidation[W, E, B] =
    a0.zipWithPar(a1)(f)

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2]
  )(
    f: (A0, A1, A2) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2).map { case ((a0, a1), a2) => f(a0, a1, a2) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3).map { case (((a0, a1), a2), a3) => f(a0, a1, a2, a3) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4).map { case ((((a0, a1), a2), a3), a4) => f(a0, a1, a2, a3, a4) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5).map { case (((((a0, a1), a2), a3), a4), a5) => f(a0, a1, a2, a3, a4, a5) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6).map { case ((((((a0, a1), a2), a3), a4), a5), a6) =>
      f(a0, a1, a2, a3, a4, a5, a6)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7).map { case (((((((a0, a1), a2), a3), a4), a5), a6), a7) =>
      f(a0, a1, a2, a3, a4, a5, a6, a7)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8).map {
      case ((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9).map {
      case (((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10).map {
      case ((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11).map {
      case (((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12).map {
      case ((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13).map {
      case (((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14).map {
      case ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15).map {
      case (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16).map {
      case ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17).map {
      case (
            ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
            a17
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18).map {
      case (
            (
              ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
              a17
            ),
            a18
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[W, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19).map {
      case (
            (
              (
                (
                  (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                  a16
                ),
                a17
              ),
              a18
            ),
            a19
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[
    W,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    B
  ](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19],
    a20: ZValidation[W, E, A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20).map {
      case (
            (
              (
                (
                  (
                    (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                    a16
                  ),
                  a17
                ),
                a18
              ),
              a19
            ),
            a20
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def validateWith[
    W,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    B
  ](
    a0: ZValidation[W, E, A0],
    a1: ZValidation[W, E, A1],
    a2: ZValidation[W, E, A2],
    a3: ZValidation[W, E, A3],
    a4: ZValidation[W, E, A4],
    a5: ZValidation[W, E, A5],
    a6: ZValidation[W, E, A6],
    a7: ZValidation[W, E, A7],
    a8: ZValidation[W, E, A8],
    a9: ZValidation[W, E, A9],
    a10: ZValidation[W, E, A10],
    a11: ZValidation[W, E, A11],
    a12: ZValidation[W, E, A12],
    a13: ZValidation[W, E, A13],
    a14: ZValidation[W, E, A14],
    a15: ZValidation[W, E, A15],
    a16: ZValidation[W, E, A16],
    a17: ZValidation[W, E, A17],
    a18: ZValidation[W, E, A18],
    a19: ZValidation[W, E, A19],
    a20: ZValidation[W, E, A20],
    a21: ZValidation[W, E, A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): ZValidation[W, E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20 <&> a21).map {
      case (
            (
              (
                (
                  (
                    (
                      (
                        ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14),
                        a15
                      ),
                      a16
                    ),
                    a17
                  ),
                  a18
                ),
                a19
              ),
              a20
            ),
            a21
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
    }
}

trait LowPriorityValidationImplicits {

  /**
   * The `CommutativeBoth` instance for `Validation`.
   */
  implicit def ZValidationCommutativeBoth[W, E]: CommutativeBoth[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      def both[A, B](fa: => ZValidation[W, E, A], fb: => ZValidation[W, E, B]): ZValidation[W, E, (A, B)] =
        fa.zipPar(fb)
    }

  /**
   * Derives a `Hash[ZValidation[W, E, A]]` given a `Hash[A]`.
   */
  implicit def ValidationHash[W, E, A: Hash]: Hash[ZValidation[W, E, A]] =
    Hash[MultiSet[E]].eitherWith(Hash[A])(_.fold(e => Left(MultiSet.fromIterable(e)), a => Right(a)))
}
