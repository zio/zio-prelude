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

import scala.annotation.nowarn

/**
 * A `PartialEquivalence[A, B, E1, E2]` describes two conversions between `A`
 * and `B` that can fail: `toPartial` converts an `A` into a `B` or fails with
 * an `E1`, and `fromPartial` converts a `B` into an `A` or fails with an `E2`.
 * An [[Equivalence]] is a `PartialEquivalence` whose conversions never fail.
 *
 * {{{
 * val parse: PartialEquivalence[String, Int, String, Nothing] =
 *   PartialEquivalence(
 *     s => if (s.nonEmpty && s.forall(_.isDigit)) Right(s.toInt) else Left(s"not a number: \$s"),
 *     i => Right(i.toString)
 *   )
 *
 * val number: Either[String, Int] = parse.toPartial("42")
 * // Right(42)
 * val canonical: Option[String] = parse.canonicalLeft("007")
 * // Some(7)
 * }}}
 */
@nowarn("msg=Unused import")
trait PartialEquivalence[A, B, +E1, +E2] { self =>
  import PartialEquivalence._
  import zio._ // for zio.EitherCompat

  /**
   * Converts an `A` into a `B`, or fails with an `E1`.
   */
  def toPartial: A => Either[E1, B]

  /**
   * Converts a `B` into an `A`, or fails with an `E2`.
   */
  def fromPartial: B => Either[E2, A]

  /**
   * A symbolic alias for `andThen`.
   */
  def >>>[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    self andThen that

  /**
   * Composes this partial equivalence with the specified one: the result
   * converts an `A` to a `B` with this partial equivalence, then the `B` to a
   * `C` with `that`, and converts back in the reverse order. The first failure
   * in either direction is the result.
   */
  def andThen[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    PartialEquivalence(
      self.toPartial andThen (_.flatMap(that.toPartial)),
      that.fromPartial andThen (_.flatMap(self.fromPartial))
    )

  /**
   * Converts `a` to a `B` and back, returning the resulting `A`, or `None`
   * when either conversion fails.
   */
  def canonicalLeft(a: A): Option[A] = canonicalLeftOrError[Any](a).fold(_ => None, Some(_))

  /**
   * Converts `a` to a `B` and back, returning the resulting `A`, or the error
   * of the first conversion that fails.
   */
  def canonicalLeftOrError[E](a: A)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, A] =
    toPartial(a).left.map(ev1).flatMap(b => fromPartial(b).left.map(ev2))

  /**
   * Converts `b` to an `A` and back, returning the resulting `B`, or `None`
   * when either conversion fails.
   */
  def canonicalRight(b: B): Option[B] = canonicalRightOrError[Any](b).fold(_ => None, Some(_))

  /**
   * Converts `b` to an `A` and back, returning the resulting `B`, or the error
   * of the first conversion that fails.
   */
  def canonicalRightOrError[E](b: B)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, B] =
    fromPartial(b).left.map(ev2).flatMap(a => toPartial(a).left.map(ev1))

  /**
   * Composes the specified partial equivalence with this one. `a compose b`
   * is `b andThen a`.
   */
  def compose[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[C, A, E3, E4]): PartialEquivalence[C, B, E3, E4] =
    that andThen self

  /**
   * Flips this partial equivalence around, so `toPartial` and `fromPartial`
   * swap places.
   */
  def flip: PartialEquivalence[B, A, E2, E1] = PartialEquivalence(fromPartial, toPartial)
}

/**
 * Constructors of `PartialEquivalence`.
 */
object PartialEquivalence {

  /**
   * Constructs a `PartialEquivalence` from two conversions that can fail.
   */
  def apply[A, B, E1, E2](
    toPartial0: A => Either[E1, B],
    fromPartial0: B => Either[E2, A]
  ): PartialEquivalence[A, B, E1, E2] =
    new PartialEquivalence[A, B, E1, E2] {
      override def toPartial: A => Either[E1, B]   = toPartial0
      override def fromPartial: B => Either[E2, A] = fromPartial0
    }

  /**
   * Extracts the `toPartial` and `fromPartial` functions of a partial
   * equivalence, for pattern matching.
   */
  def unapply[A, B, E1, E2](self: PartialEquivalence[A, B, E1, E2]): Some[(A => Either[E1, B], B => Either[E2, A])] =
    Some((self.toPartial, self.fromPartial))

  /**
   * The identity equivalence, which converts every value to itself. It is
   * `Equivalence.identity`.
   */
  def identity[A]: Equivalence[A, A] = Equivalence.identity[A]

}
