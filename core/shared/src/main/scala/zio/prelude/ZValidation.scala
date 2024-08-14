package zio.prelude

import zio.prelude.ZValidation._
import zio.{Cause, Chunk, IO, NonEmptyChunk, ZIO, Zippable}

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
  final def <&>[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B])(implicit
    zippable: Zippable[A, B]
  ): ZValidation[W1, E1, zippable.Out] =
    zipPar(that)

  /**
   * A symbolic alias for `log`.
   */
  final def ??[W1 >: W](w1: W1): ZValidation[W1, E, A] =
    log(w1)

  /**
   * Maps the successful value of this `ZValidation` to the specified constant
   * value.
   */
  final def as[B](b: B): ZValidation[W, E, B] =
    map(_ => b)

  /**
   * Maps the error value of this `ZValidation` to the specified constant
   * value.
   */
  final def asError[E2](e: E2): ZValidation[W, E2, A] =
    mapError(_ => e)

  /**
   * Returns whether this `ZValidation` and the specified `ZValidation` are
   * equal to each other.
   */
  override final def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: ZValidation[_, _, _]    => self.toEitherMultiSet == that.toEitherMultiSet
      case _                             => false
    }

  override final def hashCode(): Int = toEitherMultiSet.hashCode()

  /**
   * Transforms the value of this `ZValidation` with the specified validation
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def flatMap[W1 >: W, E1 >: E, B](f: A => ZValidation[W1, E1, B]): ZValidation[W1, E1, B] =
    self match {
      case failure: Failure[_, _] => failure
      case Success(w, a)          =>
        f(a) match {
          case Failure(w1, e) => Failure(w ++ w1, e)
          case Success(w1, b) => Success(w ++ w1, b)
        }
    }

  /**
   * Returns a ZValidation that is this ZValidation if failing or the inner ZValidation if the outer one succeeds.
   * In particular, the sequential aspect of this combinator precludes combining error values of outer and inner ZValidations.
   * This method can be used to "flatten" nested ZValidation.
   */
  final def flatten[W1 >: W, E1 >: E, B](implicit ev1: A <:< ZValidation[W1, E1, B]): ZValidation[W1, E1, B] =
    self.flatMap(a => ev1(a))

  /**
   * Transforms the value of this `ZValidation` with the specified effectual
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def forEach[F[+_]: IdentityBoth: Covariant, B](f: A => F[B]): F[ZValidation[W, E, B]] =
    self match {
      case failure: Failure[_, _] => failure.succeed[F]
      case Success(w, a)          => f(a).map(Success(w, _))
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
   * Returns the value, because no error has occurred.
   */
  final def get(implicit ev: E <:< Nothing): A = self.asInstanceOf[Success[W, A]].value

  /**
   * Returns the value of the log.
   */
  final def getLog: Chunk[W] =
    self match {
      case Failure(w, _) => w
      case Success(w, _) => w
    }

  /**
   * Returns the value, if successful, or the provided `fallback` value.
   */
  final def getOrElse[A1 >: A](fallback: => A1): A1 =
    self match {
      case Failure(_, _) => fallback
      case Success(_, a) => a
    }

  /**
   * Returns the successful value or handles the errors that have accumulated.
   */
  final def getOrElseWith[A1 >: A](f: NonEmptyChunk[E] => A1): A1 =
    self match {
      case Failure(_, e) => f(e)
      case Success(_, a) => a
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
      case failure: Failure[_, _] => failure
      case Success(w, a)          => Success(w, f(a))
    }

  /**
   * Transforms the error value of this `ZValidation` with the specified
   * function.
   */
  final def mapError[E2](f: E => E2): ZValidation[W, E2, A] =
    self match {
      case Failure(w, e)          => Failure(w, e.map(f))
      case success: Success[_, _] => success
    }

  /**
   * Transforms all the error values of this `ZValidation` with the specified
   * function.
   */
  final def mapErrorAll[E2](f: NonEmptyChunk[E] => NonEmptyChunk[E2]): ZValidation[W, E2, A] =
    self match {
      case Failure(w, e)          => Failure(w, f(e))
      case success: Success[_, _] => success
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

  final def orElse[W1 >: W, E1, A1 >: A](that: => ZValidation[W1, E1, A1]): ZValidation[W1, E1, A1] =
    self match {
      case Failure(log, _)        => that.mapLogAll(log ++ _)
      case success: Success[_, _] => success
    }

  final def orElseLog[W1 >: W, E1, A1 >: A](
    that: => ZValidation[W1, E1, A1]
  )(implicit ev: E <:< W1): ZValidation[W1, E1, A1] =
    self match {
      case Failure(log, errors)   => that.mapLogAll(log ++ errors.map(ev) ++ _)
      case success: Success[_, _] => success
    }

  /**
   * Applies the provided validation function to the successful value of this `ZValidation` without altering the result.
   * This method can be used to execute side effects or additional validations that do not transform the primary value
   * but may modify the log or error. If this `ZValidation` is a failure, it remains unchanged.
   */
  final def tap[W1 >: W, E1 >: E](f: A => ZValidation[W1, E1, Any]): ZValidation[W1, E1, A] =
    self.flatMap(a => f(a).as(a))

  /**
   * Transforms this `ZValidation` to an `Either`, discarding the log.
   */
  final def toEither: Either[NonEmptyChunk[E], A] =
    fold(Left(_), Right(_))

  /**
   * Transforms this `ZValidation` to an `Either`, aggregating errors using provided `Associative` instance, discarding the log.
   */
  final def toEitherAssociative[E1 >: E](implicit A: Associative[E1]): Either[E1, A] =
    self match {
      case Failure(_, errors) => Left(errors.reduceMap[E1](identity))
      case Success(_, value)  => Right(value)
    }

  /**
   * Transforms this `ZValidation` to an `Either`, discarding the order in which the errors occurred and discarding the log.
   */
  final def toEitherMultiSet: Either[NonEmptyMultiSet[E], A] =
    self match {
      case failure: Failure[_, _] => Left(failure.errorsUnordered)
      case Success(_, value)      => Right(value)
    }

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
      nec => ZIO.failCause(nec.reduceMapLeft(e => zio.Cause.fail(e))((c, e) => zio.Cause.Both(c, zio.Cause.fail(e)))),
      ZIO.succeed(_)
    )

  /**
   * Converts this `ZValidation` into a `ZIO` effect and exposes all parallel
   * errors in a single call, discarding the log.
   */
  final def toZIOParallelErrors: IO[NonEmptyChunk[E], A] =
    self.fold(es => ZIO.refailCause(Cause.fail(es)), ZIO.succeed(_))

  /**
   * Transforms this `ZValidation` to an `ZIO` effect, aggregating errors using provided `Associative` instance, discarding the log.
   */
  final def toZIOAssociative[E1 >: E](implicit A: Associative[E1]): IO[E1, A] =
    self.fold(
      nec => ZIO.fail(nec.reduceMap[E1](identity)),
      ZIO.succeed(_)
    )

  /**
   * Transforms the successful output of this `ZValidation` into a `Unit`, effectively discarding the original success value
   * while preserving any accumulated warnings or errors. This can be useful when the outcome of the validation process is not
   * needed, but the side effects (e.g., logging or error accumulation) are important.
   */
  final def unit: ZValidation[W, E, Unit] =
    self.as(())

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
  final def zipPar[W1 >: W, E1 >: E, B](that: ZValidation[W1, E1, B])(implicit
    zippable: Zippable[A, B]
  ): ZValidation[W1, E1, zippable.Out] =
    zipWithPar(that)(zippable.zip(_, _))

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

  final case class Failure[+W, +E](log: Chunk[W], errors: NonEmptyChunk[E]) extends ZValidation[W, E, Nothing] {
    lazy val errorsUnordered: NonEmptyMultiSet[E] = NonEmptyMultiSet.fromIterable(errors.head, errors.tail)
  }
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
    Equal[Either[NonEmptyMultiSet[E], A]].contramap(_.toEitherMultiSet)

  /**
   * The `DeriveEqual` instance for `ZValidation`.
   */
  implicit def ZValidationDeriveEqual[W, E]: DeriveEqual[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
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
  implicit def ZValidationFailureDeriveEqual[W, A: Equal]
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
   * The `IdentityFlatten` instance for `ZValidation`.
   */
  implicit def ZValidationIdentityFlatten[W, E]: IdentityFlatten[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZValidation[W, E, a] })#lambda] {
      val any: Validation[Nothing, Any]                                                  =
        ZValidation.unit
      def flatten[A](ffa: ZValidation[W, E, ZValidation[W, E, A]]): ZValidation[W, E, A] =
        ffa.flatten
    }

  /**
   * Derives a `PartialOrd[ZValidation[W, E, A]]` given an `Ord[E]` and an `Ord[A]`.
   */
  implicit def ZValidationPartialOrd[W, E, A: PartialOrd]: PartialOrd[ZValidation[W, E, A]] =
    PartialOrd[Either[NonEmptyMultiSet[E], A]].contramap(_.toEitherMultiSet)

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
   * that either returns the values of all of them, if they all succeed, or
   * else fails with all of their errors.
   */
  def validateAll[F[+_]: ForEach, W, E, A](validations: F[ZValidation[W, E, A]]): ZValidation[W, E, F[A]] =
    validations.flip

  /**
   * Combine a set of `ZValidation` values into a single `ZValidation` that
   * either returns the values of all of them, if they all succeed, or else
   * fails with all of their errors.
   */
  def validateAll[W, E, A](validations: Set[ZValidation[W, E, A]]): ZValidation[W, E, Set[A]] =
    validateAll[Iterable, W, E, A](validations).map(_.toSet)

  /**
   * Constructs a `ZValidation` that fails with the specified error.
   */
  def fail[E](error: E): Validation[E, Nothing] =
    Failure(Chunk.empty, NonEmptyChunk(error))

  /**
   * Constructs a `ZValidation` that fails with the specified `NonEmptyChunk`
   * of errors.
   */
  def failNonEmptyChunk[E](errors: NonEmptyChunk[E]): Validation[E, Nothing] =
    Failure(Chunk.empty, errors)

  /**
   * Constructs a `ZValidation` from an `Either`.
   */
  def fromEither[E, A](value: Either[E, A]): Validation[E, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `ZValidation` from an `Either` that fails with a
   * `NonEmptyChunk` of errors.
   */
  def fromEitherNonEmptyChunk[E, A](value: Either[NonEmptyChunk[E], A]): Validation[E, A] =
    value.fold(failNonEmptyChunk, succeed)

  /**
   * Constructs a `ZValidation` from an `Option`.
   */
  def fromOption[A](value: Option[A]): Validation[Unit, A] =
    value.fold[Validation[Unit, A]](fail(()))(succeed)

  /**
   * Constructs a `Validation` from an `Option`, failing with the error
   * provided.
   */
  def fromOptionWith[E, A](error: => E)(value: Option[A]): Validation[E, A] =
    value.fold[Validation[E, A]](fail(error))(succeed)

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
   * Converts an `Option` to a `ZValidation`, treating `None` as a success with
   * no information and `Some` as a failure with the specified error.
   */
  def noneOrFail[E](option: Option[E]): Validation[E, Unit] =
    option match {
      case None    => ZValidation.unit
      case Some(e) => ZValidation.fail(e)
    }

  /**
   * Validates each element in a collection, collecting the results into a
   * collection of failed results and a collection of successful results.
   */
  def partition[F[+_]: ForEach: IdentityBoth: IdentityEither, W, E, A, B](
    fa: F[A]
  )(f: A => ZValidation[W, E, B]): ZValidation[W, Nothing, (F[E], F[B])] = {
    implicit val leftIdentity: Identity[F[E]]  = Identity.fromIdentityEitherCovariant
    implicit val rightIdentity: Identity[F[B]] = Identity.fromIdentityEitherCovariant

    val (w, es, bs) = fa.foldMap { a =>
      f(a) match {
        case Failure(w, es) => (w, es.foldMap(_.succeed[F]), IdentityEither[F].none)
        case Success(w, b)  => (w, IdentityEither[F].none, b.succeed[F])
      }
    }

    ZValidation.Success(w, (es, bs))
  }

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
    (a0 <&> a1 <&> a2).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13).map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20)
      .map(f.tupled)

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
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20 <&> a21)
      .map(f.tupled)
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
  implicit def ZValidationHash[W, E, A: Hash]: Hash[ZValidation[W, E, A]] =
    Hash[Either[NonEmptyMultiSet[E], A]].contramap(_.toEitherMultiSet)
}
