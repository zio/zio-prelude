package zio.prelude

import scala.util.Try

import zio.prelude.Validation._
import zio.test.Assertion
import zio.{ IO, NonEmptyChunk, ZIO }

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
   * Returns whether this `Validation` and the specified `Validation` are equal
   * to each other.
   */
  override final def equals(that: Any): Boolean =
    (self, that) match {
      case (self, that: AnyRef) if self.eq(that) => true
      case (Failure(es), Failure(e1s))           => es.groupBy(identity) == e1s.groupBy(identity)
      case (Success(a), Success(a1))             => a == a1
      case _                                     => false
    }

  /**
   * Transforms the value of this `Validation` with the specified validation
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def flatMap[E1 >: E, B](f: A => Validation[E1, B]): Validation[E1, B] =
    self match {
      case Failure(es) => Failure(es)
      case Success(a)  => f(a)
    }

  /**
   * Transforms the value of this `Validation` with the specified effectual
   * function if it is a success or returns the value unchanged otherwise.
   */
  final def foreach[F[+_], B](f: A => F[B])(implicit ib: IdentityBoth[F], c: Covariant[F]): F[Validation[E, B]] =
    self match {
      case Failure(es) => Failure(es).succeed(ib, c)
      case Success(a)  => f(a).map(Success(_))
    }

  /**
   * Folds over the error and success values of this `Validation`.
   */
  final def fold[B](failure: NonEmptyChunk[E] => B, success: A => B): B =
    self match {
      case Failure(es) => failure(es)
      case Success(a)  => success(a)
    }

  /**
   * Transforms the successful value of this `Validation` with the specified
   * function.
   */
  final def map[B](f: A => B): Validation[E, B] =
    self match {
      case Success(a)           => Success(f(a))
      case failure @ Failure(_) => failure
    }

  /**
   * Transforms the error value of this `Validation` with the specified
   * function.
   */
  final def mapError[E1](f: E => E1): Validation[E1, A] =
    self match {
      case Failure(es)          => Failure(es.map(f))
      case success @ Success(_) => success
    }

  /**
   * Transforms this `Validation` to an `Either`.
   */
  final def toEither[E1 >: E]: Either[NonEmptyChunk[E1], A] =
    fold(Left(_), Right(_))

  /**
   * Transforms this `Validation` to an `Option`, discarding information about
   * the errors.
   */
  final def toOption: Option[A] =
    fold(_ => None, Some(_))

  /**
   * Transforms this `Validation` to a `Try`, discarding all but the first error.
   */
  final def toTry(implicit ev: E <:< Throwable): scala.util.Try[A] =
    fold(e => scala.util.Failure(ev(e.head)), scala.util.Success(_))

  /**
   * Converts this `Validation` into a `ZIO` effect.
   */
  final def toZIO: IO[NonEmptyChunk[E], A] = ZIO.fromEither(self.toEither)

  /**
   * A variant of `zipPar` that keeps only the left success value, but returns
   * a failure with all errors if either this `Validation` or the specified
   * `Validation` fail.
   */
  final def zipParLeft[E1 >: E, B](that: Validation[E1, B]): Validation[E1, A] =
    zipWithPar(that)((a, _) => a)

  /**
   * A variant of `zipPar` that keeps only the right success value, but returns
   * a failure with all errors if either this `Validation` or the specified
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
      case (Failure(es), Failure(e1s)) => Failure(es ++ e1s)
      case (failure @ Failure(_), _)   => failure
      case (_, failure @ Failure(_))   => failure
      case (Success(a), Success(b))    => Success(f(a, b))
    }
}

object Validation extends LowPriorityValidationImplicits {

  final case class Failure[+E](errors: NonEmptyChunk[E]) extends Validation[E, Nothing]
  final case class Success[+A](value: A)                 extends Validation[Nothing, A]

  /**
   * The `Covariant` instance for `Validation`.
   */
  implicit def ValidationCovariant[E]: Covariant[({ type lambda[+x] = Validation[E, x] })#lambda] =
    new Covariant[({ type lambda[+x] = Validation[E, x] })#lambda] {
      def map[A, B](f: A => B): Validation[E, A] => Validation[E, B] =
        _.map(f)
    }

  /**
   * The `Bicovariant` instance for `Validation`.
   */
  implicit def ValidationBicovariant[E]: Bicovariant[Validation] =
    new Bicovariant[Validation] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): Validation[A, B] => Validation[AA, BB] =
        _.map(g).mapError(f)
    }

  /**
   * Derives a `Debug[Validation[E, A]]` given a `Debug[E]` and a `Debug[A]`.
   */
  implicit def ValidationDebug[E: Debug, A: Debug]: Debug[Validation[E, A]] = {
    case Failure(es) => Debug.Repr.VConstructor(List("zio", "prelude"), "Validation.Failure", List(es.debug))
    case Success(a)  => Debug.Repr.VConstructor(List("zio", "prelude"), "Validation.Success", List(a.debug))
  }

  /**
   * Derives an `Equal[Validation[E, A]]` given an `Equal[E]` and an
   * `Equal[A]`.
   */
  implicit def ValidationEqual[E, A: Equal]: Equal[Validation[E, A]] =
    Equal.make {
      case (Failure(es), Failure(e1s)) => es.groupBy(identity) == e1s.groupBy(identity)
      case (Success(a), Success(a1))   => a === a1
      case _                           => false
    }

  /**
   * The `DeriveEqual` instance for `Validation`.
   */
  implicit def ValidationDeriveEqual[E: Equal]: DeriveEqual[({ type lambda[+x] = Validation[E, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = Validation[E, x] })#lambda] {
      def derive[A: Equal]: Equal[Validation[E, A]] =
        ValidationEqual
    }

  /**
   * The `Covariant` instance for `Validation` with respect to its error type.
   */
  implicit def ValidationFailureCovariant[A]
    : Covariant[({ type lambda[+x] = newtypes.Failure[Validation[x, A]] })#lambda] =
    new Covariant[({ type lambda[+x] = newtypes.Failure[Validation[x, A]] })#lambda] {
      def map[E, E1](f: E => E1): newtypes.Failure[Validation[E, A]] => newtypes.Failure[Validation[E1, A]] =
        validation => newtypes.Failure.wrap(newtypes.Failure.unwrap(validation).mapError(f))
    }

  /**
   * The `DeriveEqual` instance for `Validation` with respect to its error type.
   */
  implicit def ValidationFailureDeriveEqual[A: Equal]
    : DeriveEqual[({ type lambda[+x] = newtypes.Failure[Validation[x, A]] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = newtypes.Failure[Validation[x, A]] })#lambda] {
      def derive[E: Equal]: Equal[newtypes.Failure[Validation[E, A]]] =
        ValidationEqual[E, A].contramap(newtypes.Failure.unwrap)
    }

  /**
   * Derives a `Hash[Validation[E, A]]` given a `Hash[E]` and a `Hash[A]`.
   */
  implicit def ValidationHash[E: Hash, A: Hash]: Hash[Validation[E, A]] =
    Hash[NonEmptyChunk[E]].eitherWith(Hash[A])(_.toEither)

  /**
   * The `CommutativeBoth` and `IdentityBoth` (and thus `AssociativeBoth`) instance for Validation.
   */
  implicit def ValidationCommutativeIdentityBoth[E]: CommutativeBoth[({ type lambda[x] = Validation[E, x] })#lambda]
    with IdentityBoth[({ type lambda[x] = Validation[E, x] })#lambda] =
    new CommutativeBoth[({ type lambda[x] = Validation[E, x] })#lambda]
      with IdentityBoth[({ type lambda[x] = Validation[E, x] })#lambda] {
      val any: Validation[Nothing, Any]                                                       =
        Validation.unit
      def both[A, B](fa: => Validation[E, A], fb: => Validation[E, B]): Validation[E, (A, B)] =
        fa.zipPar(fb)
    }

  /**
   * The `Traversable` instance for `Validation`.
   */
  implicit def ValidationTraversable[E]: Traversable[({ type lambda[+x] = Validation[E, x] })#lambda] =
    new Traversable[({ type lambda[+x] = Validation[E, x] })#lambda] {
      def foreach[F[+_]: IdentityBoth: Covariant, A, B](fa: Validation[E, A])(f: A => F[B]): F[Validation[E, B]] =
        fa.foreach(f)
    }

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
    Failure(NonEmptyChunk(error))

  /**
   * Constructs a `Validation` from a value and an assertion about that value.
   * The resulting `Validation` will be a success if the value satisfies the
   * assertion or else will contain a string rendering describing how the
   * value did not satisfy the assertion.
   */
  def fromAssert[A](value: A)(assertion: Assertion[A]): Validation[String, A] =
    if (assertion.test(value)) succeed(value)
    else fail(s"$value did not satisfy ${assertion.render}")

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
   * Constructs a `Validation` from a predicate, failing with None.
   */
  def fromPredicate[A](value: A)(f: A => Boolean): Validation[None.type, A] =
    fromPredicateWith(None, value)(f)

  /**
   * Constructs a `Validation` from a predicate, failing with the error provided.
   */
  def fromPredicateWith[E, A](error: E, value: A)(f: A => Boolean): Validation[E, A] =
    if (f(value)) Validation.succeed(value)
    else Validation.fail(error)

  /**
   * Constructs a `Validation` from a `Try`.
   */
  def fromTry[A](value: => Try[A]): Validation[Throwable, A] =
    value match {
      case util.Failure(exception) => fail(exception)
      case util.Success(value)     => succeed(value)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, B](a0: Validation[E, A0], a1: Validation[E, A1])(f: (A0, A1) => B): Validation[E, B] =
    a0.zipWithPar(a1)(f)

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, B](a0: Validation[E, A0], a1: Validation[E, A1], a2: Validation[E, A2])(
    f: (A0, A1, A2) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2).map { case ((a0, a1), a2) => f(a0, a1, a2) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3).map { case (((a0, a1), a2), a3) => f(a0, a1, a2, a3) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4).map { case ((((a0, a1), a2), a3), a4) => f(a0, a1, a2, a3, a4) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5).map { case (((((a0, a1), a2), a3), a4), a5) => f(a0, a1, a2, a3, a4, a5) }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6).map { case ((((((a0, a1), a2), a3), a4), a5), a6) =>
      f(a0, a1, a2, a3, a4, a5, a6)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7).map { case (((((((a0, a1), a2), a3), a4), a5), a6), a7) =>
      f(a0, a1, a2, a3, a4, a5, a6, a7)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8).map {
      case ((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9).map {
      case (((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10).map {
      case ((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11).map {
      case (((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12).map {
      case ((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13).map {
      case (((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14).map {
      case ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15).map {
      case (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16).map {
      case ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    }

  /**
   * Combines the results of the specified `Validation` values using the
   * function `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): Validation[E, B] =
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
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): Validation[E, B] =
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
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): Validation[E, B] =
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
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): Validation[E, B] =
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
  def mapParN[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20],
    a21: Validation[E, A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): Validation[E, B] =
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

  /**
   * Constructs a `Validation` that succeeds with the specified value.
   */
  def succeed[A](value: A): Validation[Nothing, A] =
    Success(value)

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1](a0: Validation[E, A0], a1: Validation[E, A1]): Validation[E, (A0, A1)] =
    a0 <&> a1

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2]
  ): Validation[E, (A0, A1, A2)] =
    mapParN(a0, a1, a2)((_, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3]
  ): Validation[E, (A0, A1, A2, A3)] =
    mapParN(a0, a1, a2, a3)((_, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4]
  ): Validation[E, (A0, A1, A2, A3, A4)] =
    mapParN(a0, a1, a2, a3, a4)((_, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5]
  ): Validation[E, (A0, A1, A2, A3, A4, A5)] =
    mapParN(a0, a1, a2, a3, a4, a5)((_, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6)((_, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7)((_, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)((_, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)((_, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)((_, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)((_, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20]
  ): Validation[E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `Validation` values into a tuple,
   * failing with the accumulation of all errors if any fail.
   */
  def tupledPar[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20],
    a21: Validation[E, A21]
  ): Validation[
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  ] =
    mapParN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * The `Validation` that succeeds with the `Unit` value.
   */
  val unit: Validation[Nothing, Unit] =
    succeed(())
}

trait LowPriorityValidationImplicits {

  /**
   * Derives an `Ord[Validation[E, A]]` given na `Ord[E]` and an `Ord[A]`.
   */
  implicit def ValidationOrd[E: Ord, A: Ord]: Ord[Validation[E, A]] =
    Ord[NonEmptyChunk[E]].eitherWith(Ord[A])(_.toEither)
}
