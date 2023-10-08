/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.These._

/**
 * `These` is a data type representing a value that may either be a `Left`
 * with an`A`, a `Right` with a `B`, or a `Both` with an `A` and a `B`.
 *
 * `These` can be useful to model certain domains where both values may be
 * present in addition to one or the other. For example, in streaming
 * applications we may want to consume values from two upstream producers
 * concurrently. Depending on the timing of the producers either one producer,
 * the other, or both producers may have values that are ready to be consumed.
 * Using `These` provides a convenient way to model all of these possibilities
 * in a single "flat" data type.
 *
 * `These` can also be useful for representing computations that may produce
 * both a value and an error. For example, transferring money between two bank
 * accounts might either succeed, fail completely if there are not sufficient
 * funds, or succeed with some warnings if the sender's account balance would
 * be very low after the transfer or the receiver's account has not been
 * verified. `These` allows modeling these types of computations and preserving
 * information regarding all errors while still potentially returning a
 * successful computation.
 */
sealed trait These[+A, +B] extends Product with Serializable { self =>

  /**
   * A symbolic alias for `zipParRight`.
   */
  final def &>[A1 >: A: Commutative, C](that: These[A1, C]): These[A1, C] =
    zipParRight(that)

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[A1 >: A: Associative, C](that: These[A1, C]): These[A1, C] =
    zipRight(that)

  /**
   * A symbolic alias for `zipParLeft`.
   */
  final def <&[A1 >: A: Commutative, C](that: These[A1, C]): These[A1, B] =
    zipParLeft(that)

  /**
   * A symbolic alias for `zipPar`.
   */
  final def <&>[A1 >: A: Commutative, C](that: These[A1, C]): These[A1, (B, C)] =
    zipPar(that)

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[A1 >: A: Associative, C](that: These[A1, C]): These[A1, B] =
    zipLeft(that)

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[A1 >: A: Associative, C](that: These[A1, C]): These[A1, (B, C)] =
    zip(that)

  /**
   * A symbolic alias for `orElseEither`.
   */
  final def <+>[A1 >: A: Associative, C](that: => These[A1, C]): These[A1, Either[B, C]] =
    orElseEither(that)

  /**
   * A symbolic alias for `flatMap`.
   */
  final def >>=[A1 >: A: Associative, C](f: B => These[A1, C]): These[A1, C] =
    flatMap(f)

  /**
   * Transforms both the `A` value and the `B` value with the specified
   * functions `f` and `g`.
   */
  final def bimap[A2, C](f: A => A2, g: B => C): These[A2, C] =
    self match {
      case Left(a)    => left(f(a))
      case Right(b)   => right(g(b))
      case Both(a, b) => both(f(a), g(b))
    }

  /**
   * Returns a new computation based on the successful result of this
   * computation. If this computation contains a success the new computation
   * will be performed, even if this computation also contains errors. Any
   * errors produced by the two computations will be combined using the
   * `Associative` instance for `A`. If this computation does not contain a
   * success then the original failure will be returned unchanged.
   */
  final def flatMap[A1 >: A: Associative, C](f: B => These[A1, C]): These[A1, C] =
    self match {
      case Left(a)    => left(a)
      case Right(b)   => f(b)
      case Both(a, b) =>
        f(b) match {
          case Left(a1)    => left(a <> a1)
          case Right(c)    => both(a, c)
          case Both(a1, c) => both(a <> a1, c)
        }
    }

  /**
   * Flattens a nested `These` computation to a single level.
   */
  final def flatten[A1 >: A, C](implicit ev: B <:< These[A1, C], ev2: Associative[A1]): These[A1, C] =
    flatMap(ev)

  /**
   * Flips the left and right values.
   */
  final def flip: These[B, A] =
    self match {
      case Left(a)    => right(a)
      case Right(b)   => left(b)
      case Both(a, b) => both(b, a)
    }

  /**
   * Folds each of the possible cases into a summary value.
   */
  final def fold[C](left: A => C, right: B => C)(both: (A, B) => C): C =
    self match {
      case Left(a)    => left(a)
      case Right(b)   => right(b)
      case Both(a, b) => both(a, b)
    }

  /**
   * Transforms the successful result of this computation with the specified
   * effectual function, leaving any error value unchanged.
   */
  final def forEach[F[+_]: IdentityBoth: Covariant, C](f: B => F[C]): F[These[A, C]] =
    self match {
      case Left(a)    => left(a).succeed[F]
      case Right(b)   => f(b).map(right)
      case Both(a, b) => f(b).map(c => both(a, c))
    }

  /**
   * Returns whether this value is a `Left`.
   */
  final def isLeft: Boolean =
    fold(_ => true, _ => false)((_, _) => false)

  /**
   * Returns whether this value is a `Right`.
   */
  final def isRight: Boolean =
    fold(_ => false, _ => true)((_, _) => false)

  /**
   * Returns whether this value is a `Both`.
   */
  final def isBoth: Boolean =
    fold(_ => false, _ => false)((_, _) => true)

  /**
   * Transforms the `B` value with the specified function.
   */
  final def map[C](f: B => C): These[A, C] =
    self match {
      case Left(a)    => left(a)
      case Right(b)   => right(f(b))
      case Both(a, b) => both(a, f(b))
    }

  /**
   * Transforms the `A` value with the specified function.
   */
  final def mapLeft[A2](f: A => A2): These[A2, B] =
    self match {
      case Left(a)    => left(f(a))
      case Right(b)   => right(b)
      case Both(a, b) => both(f(a), b)
    }

  final def orElse[A1 >: A: Associative, B1 >: B](that: => These[A1, B1]): These[A1, B1] =
    orElseEither(that).map(_.merge)

  final def orElseEither[A1 >: A: Associative, C](that: => These[A1, C]): These[A1, Either[B, C]] =
    self match {
      case Left(a)    =>
        that match {
          case Left(a1)    => left(a <> a1)
          case Right(c)    => both(a, scala.util.Right(c))
          case Both(a1, c) => both(a <> a1, scala.util.Right(c))
        }
      case Right(b)   => right(scala.util.Left(b))
      case Both(a, b) => both(a, scala.util.Left(b))
    }

  /**
   * Converts this value to an `Either` containing a `Right` if this value
   * contains a success or a `Left` otherwise, discarding information about any
   * errors in the case of success.
   */
  final def toEither: Either[A, B] =
    fold(a => scala.util.Left(a), b => scala.util.Right(b))((_, b) => scala.util.Right(b))

  /**
   * Converts this value to an `Option` containing `Some` if this value
   * contains a success or `None` otherwise, discarding information about any
   * errors in the case of success.
   */
  final def toOption: Option[B] =
    fold(_ => None, b => Some(b))((_, b) => Some(b))

  /**
   * Converts this value to a validation success if this value contains a
   * success or a validation failure otherwise, discarding information about
   * any errors in the case of success.
   */
  final def toValidation: Validation[A, B] =
    fold(a => Validation.fail(a), b => Validation.succeed(b))((_, b) => Validation.succeed(b))

  /**
   * Converts this value to a validation success if this value contains a
   * success or a validation failure otherwise, submerging multiple errors into
   * the structure of the validation and discarding information about any
   * errors in the case of success.
   */
  final def toValidationNonEmptyChunk[A1](implicit ev: A <:< NonEmptyChunk[A1]): Validation[A1, B] =
    fold(a => Validation.failNonEmptyChunk(ev(a)), b => Validation.succeed(b))((_, b) => Validation.succeed(b))

  /**
   * Combines this computation sequentially with that computation, combining
   * their results into a tuple. If this computation does not return a success
   * that computation will not be performed. If both computations are performed
   * any errors will be combined using the `Associative` instance for `A`.
   */
  final def zip[A1 >: A: Associative, C, D](that: These[A1, C]): These[A1, (B, C)] =
    zipWith(that)((_, _))

  /**
   * Combines this computation sequentially with that computation, returning
   * only the left value. If this computation does not return a success that
   * computation will not be performed. If both computations are performed any
   * errors will be combined using the `Associative` instance for `A`.
   */
  final def zipLeft[A1 >: A: Associative, C, D](that: These[A1, C]): These[A1, B] =
    zipWith(that)((b, _) => b)

  /**
   * Combines this computation with that computation into a tuple, performing
   * both computations even if this computation does not return a success and
   * combining any errors using the `Commutative` instance for `A`.
   */
  final def zipPar[A1 >: A: Commutative, C, D](that: These[A1, C]): These[A1, (B, C)] =
    zipWithPar(that)((_, _))

  /**
   * Combines this computation with that computation and returning only the
   * left value, performing both computations even if this computation does not
   * return a success and combining any errors using the `Commutative` instance
   * for `A`.
   */
  final def zipParLeft[A1 >: A: Commutative, C, D](that: These[A1, C]): These[A1, B] =
    zipWithPar(that)((b, _) => b)

  /**
   * Combines this computation with that computation and returning only the
   * right value, performing both computations even if this computation does
   * not return a success and combining any errors using the `Commutative`
   * instance for `A`.
   */
  final def zipParRight[A1 >: A: Commutative, C, D](that: These[A1, C]): These[A1, C] =
    zipWithPar(that)((_, c) => c)

  /**
   * Combines this computation sequentially with that computation, returning
   * only the right value. If this computation does not return a success that
   * computation will not be performed. If both computations are performed any
   * errors will be combined using the `Associative` instance for `A`.
   */
  final def zipRight[A1 >: A: Associative, C, D](that: These[A1, C]): These[A1, C] =
    zipWith(that)((_, c) => c)

  /**
   * Combines this computation sequentially with that computation, combining
   * their results with the specified function. If this computation does
   * not return a success that computation will not be performed. If both
   * computations are performed any errors will be combined using the
   * `Associative` instance for `A`.
   */
  final def zipWith[A1 >: A: Associative, C, D](that: These[A1, C])(f: (B, C) => D): These[A1, D] =
    self.flatMap(b => that.map(c => f(b, c)))

  /**
   * Combines this computation with that computation using the specified
   * function, performing both computations even if this computation does not
   * return a success and combining any errors using the `Commutative`
   * instance for `A`.
   */
  final def zipWithPar[A1 >: A: Commutative, C, D](that: These[A1, C])(f: (B, C) => D): These[A1, D] =
    (self, that) match {
      case (Left(a), Left(a1))       => left(a <> a1)
      case (Left(a), Right(_))       => left(a)
      case (Left(a), Both(a1, _))    => left(a <> a1)
      case (Right(_), Left(a))       => left(a)
      case (Right(b), Right(c))      => right(f(b, c))
      case (Right(b), Both(a, c))    => both(a, f(b, c))
      case (Both(a, _), Left(a1))    => left(a <> a1)
      case (Both(a, b), Right(c))    => both(a, f(b, c))
      case (Both(a, b), Both(a1, c)) => both(a <> a1, f(b, c))
    }

  def reduceMap[C](f: A => C, g: B => C)(implicit C: Associative[C]): C =
    self match {
      case Left(l)    => f(l)
      case Right(r)   => g(r)
      case Both(l, r) => f(l) combine g(r)
    }

}

object These {

  final case class Left[+A](value: A)              extends These[A, Nothing]
  final case class Right[+B](value: B)             extends These[Nothing, B]
  final case class Both[+A, +B](left: A, right: B) extends These[A, B]

  /**
   * Derives an `Associative[These[A, B]]` given an `Associative[A]` and an `Associative[B]`.
   */
  implicit def TheseAssociative[A: Associative, B: Associative]: Associative[These[A, B]] =
    Associative.make {
      case (Left(a), Left(a1))        => left(a <> a1)
      case (Left(a), Right(b))        => both(a, b)
      case (Left(a), Both(a1, b))     => both(a <> a1, b)
      case (Right(b), Left(a))        => both(a, b)
      case (Right(b), Right(b1))      => right(b <> b1)
      case (Right(b), Both(a, b1))    => both(a, b <> b1)
      case (Both(a, b), Left(a1))     => both(a <> a1, b)
      case (Both(a, b), Right(b1))    => both(a, b <> b1)
      case (Both(a, b), Both(a1, b1)) => both(a <> a1, b <> b1)
    }

  /**
   * The `AssociativeEither` instance for `These`.
   */
  implicit def TheseAssociativeEither[A: Associative]: AssociativeEither[({ type lambda[+b] = These[A, b] })#lambda] =
    new AssociativeEither[({ type lambda[+b] = These[A, b] })#lambda] {
      def either[B, C](fa: => These[A, B], fb: => These[A, C]): These[A, Either[B, C]] =
        fa.orElseEither(fb)
    }

  /**
   * Derives a `Commutative[These[A, B]]` given a `Commutative[A]` and a `Commutative[B]`.
   */
  implicit def TheseCommutative[A: Commutative, B: Commutative]: Commutative[These[A, B]] =
    Commutative.make {
      case (Left(a), Left(a1))        => left(a <> a1)
      case (Left(a), Right(b))        => both(a, b)
      case (Left(a), Both(a1, b))     => both(a <> a1, b)
      case (Right(b), Left(a))        => both(a, b)
      case (Right(b), Right(b1))      => right(b <> b1)
      case (Right(b), Both(a, b1))    => both(a, b <> b1)
      case (Both(a, b), Left(a1))     => both(a <> a1, b)
      case (Both(a, b), Right(b1))    => both(a, b <> b1)
      case (Both(a, b), Both(a1, b1)) => both(a <> a1, b <> b1)
    }

  /**
   * The `CommutativeBoth` instance for `These`.
   */
  implicit def TheseCommutativeBoth[A: Commutative]: CommutativeBoth[({ type lambda[+b] = These[A, b] })#lambda] =
    new CommutativeBoth[({ type lambda[+b] = These[A, b] })#lambda] {
      def both[B, C](fa: => These[A, B], fb: => These[A, C]): These[A, (B, C)] =
        fa.zipPar(fb)
    }

  /**
   * The `Covariant` instance for `These`.
   */
  implicit def TheseCovariant[A]: Covariant[({ type lambda[+b] = These[A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = These[A, b] })#lambda] {
      def map[B, C](f: B => C): These[A, B] => These[A, C] =
        _.map(f)
    }

  /**
   * Derives a `Debug[These[A, B]]` given a `Debug[A]` and a `Debug[B]`.
   */
  implicit def TheseDebug[A: Debug, B: Debug]: Debug[These[A, B]] = {
    case Left(a)    => Debug.Repr.VConstructor(List("zio", "prelude"), "These.Left", List(a.debug))
    case Right(b)   => Debug.Repr.VConstructor(List("zio", "prelude"), "These.Right", List(b.debug))
    case Both(a, b) => Debug.Repr.VConstructor(List("zio", "prelude"), "These.Both", List(a.debug, b.debug))
  }

  /**
   * Derives an `Equal[These[A, B]]` given an `Equal[A]` and an `Equal[B]`.
   */
  implicit def TheseEqual[A: Equal, B: Equal]: Equal[These[A, B]] =
    Equal.make {
      case (Left(a), Left(a1))        => a === a1
      case (Right(b), Right(b1))      => b === b1
      case (Both(a, b), Both(a1, b1)) => a === a1 && b === b1
      case _                          => false
    }

  /**
   * The `DeriveEqual` instance for `These`.
   */
  implicit def TheseDeriveEqual[A: Equal]: DeriveEqual[({ type lambda[+b] = These[A, b] })#lambda] =
    new DeriveEqual[({ type lambda[+b] = These[A, b] })#lambda] {
      def derive[B: Equal]: Equal[These[A, B]] =
        TheseEqual
    }

  /**
   * The `ForEach` instance for `These`.
   */
  implicit def TheseForEach[A]: ForEach[({ type lambda[+b] = These[A, b] })#lambda] =
    new ForEach[({ type lambda[+b] = These[A, b] })#lambda] {
      def forEach[F[+_]: IdentityBoth: Covariant, B, C](fa: These[A, B])(f: B => F[C]): F[These[A, C]] =
        fa.forEach(f)
    }

  /**
   * Derives a `Hash[These[A, B]]` given a `Hash[A]` and a `Hash[B]`.
   */
  implicit def TheseHash[A: Hash, B: Hash]: Hash[These[A, B]] =
    Hash.make(
      _.bimap(_.hash, _.hash).hashCode,
      {
        case (Left(a), Left(a1))        => a === a1
        case (Right(b), Right(b1))      => b === b1
        case (Both(a, b), Both(a1, b1)) => a === a1 && b === b1
        case _                          => false
      }
    )

  /**
   * The `IdentityBoth` instance for `These`.
   */
  implicit def TheseIdentityBoth[A: Associative]: IdentityBoth[({ type lambda[+b] = These[A, b] })#lambda] =
    new IdentityBoth[({ type lambda[+b] = These[A, b] })#lambda] {
      val any: These[Nothing, Any]                                             =
        These.unit
      def both[B, C](fa: => These[A, B], fb: => These[A, C]): These[A, (B, C)] =
        fa.zip(fb)
    }

  /**
   * The `IdentityFlatten` instance for `These`.
   */
  implicit def TheseIdentityFlatten[A: Associative]: IdentityFlatten[({ type lambda[+b] = These[A, b] })#lambda] =
    new IdentityFlatten[({ type lambda[+b] = These[A, b] })#lambda] {
      val any: These[Nothing, Any]                            =
        These.unit
      def flatten[B](ffa: These[A, These[A, B]]): These[A, B] =
        ffa.flatten
    }

  /**
   * Constructs a `Both` with an `A` value and a `B` value.
   */
  def both[A, B](a: A, b: B): These[A, B] =
    Both(a, b)

  /**
   * Constructs a `Both` with an `A` value and a `B` value, wrapping the `A`
   * value in a `NonEmptyChunk`. This is useful when using `These` to model
   * computations that may both succeed and fail to use the `Associative`
   * instance for `NonEmptyChunk` to accumulate all errors.
   */
  def bothNonEmptyChunk[A, B](a: A, b: B): These[NonEmptyChunk[A], B] =
    both(NonEmptyChunk(a), b)

  /**
   * Constructs a `These` from an `Either` value.
   */
  def fromEither[A, B](eab: Either[A, B]): These[A, B] =
    eab.fold(left, right)

  /**
   * Constructs a `These` from an `Option` value.
   */
  def fromOption[A](ob: Option[A]): These[Unit, A] =
    ob.fold[These[Unit, A]](left(()))(right)

  /**
   *  Constructor from two options to an option of These.
   */
  def fromOptions[A, B](opA: Option[A], opB: Option[B]): Option[These[A, B]] =
    (opA, opB) match {
      case (Some(a), Some(b)) => Some(Both(a, b))
      case (Some(a), _)       => Some(Left(a))
      case (_, Some(b))       => Some(Right(b))
      case _                  => None
    }

  /**
   * Constructs a `These` from a `Validation` value.
   */
  def fromValidation[E, A](validation: Validation[E, A]): These[NonEmptyChunk[E], A] =
    validation.fold(left, right)

  /**
   * Constructs a `Left` with an `A` value.
   */
  def left[A](a: A): These[A, Nothing] =
    Left(a)

  /**
   * Constructs a `Left` with an `A` value, wrapping the `A` value in a
   * `NonEmptyChunk`. This is useful when using `These` to model computations
   * that may both succeed and fail to use the `Associative` instance for
   * `NonEmptyChunk` to accumulate multiple errors.
   */
  def leftNonEmptyChunk[A](a: A): These[NonEmptyChunk[A], Nothing] =
    left(NonEmptyChunk(a))

  /**
   * Constructs a `Right` with an `A` value.
   */
  def right[A](a: A): These[Nothing, A] =
    Right(a)

  /**
   * A `Right` with the `Unit` value.
   */
  val unit: These[Nothing, Unit] =
    right(())
}
