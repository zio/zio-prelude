/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

import com.github.ghik.silencer.silent

@silent("Unused import")
trait PartialEquivalence[A, B, +E1, +E2] { self =>
  import PartialEquivalence._
  import zio._ // for zio.EitherCompat

  def toPartial: A => Either[E1, B]

  def fromPartial: B => Either[E2, A]

  def >>>[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    self andThen that

  def andThen[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    PartialEquivalence(
      self.toPartial andThen (_.flatMap(that.toPartial)),
      that.fromPartial andThen (_.flatMap(self.fromPartial))
    )

  def canonicalLeft(a: A): Option[A] = canonicalLeftOrError[Any](a).fold(_ => None, Some(_))

  def canonicalLeftOrError[E](a: A)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, A] =
    toPartial(a).left.map(ev1).flatMap(b => fromPartial(b).left.map(ev2))

  def canonicalRight(b: B): Option[B] = canonicalRightOrError[Any](b).fold(_ => None, Some(_))

  def canonicalRightOrError[E](b: B)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, B] =
    fromPartial(b).left.map(ev2).flatMap(a => toPartial(a).left.map(ev1))

  def compose[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[C, A, E3, E4]): PartialEquivalence[C, B, E3, E4] =
    that andThen self

  def flip: PartialEquivalence[B, A, E2, E1] = PartialEquivalence(fromPartial, toPartial)
}

object PartialEquivalence {

  def apply[A, B, E1, E2](
    toPartial0: A => Either[E1, B],
    fromPartial0: B => Either[E2, A]
  ): PartialEquivalence[A, B, E1, E2] =
    new PartialEquivalence[A, B, E1, E2] {
      override def toPartial: A => Either[E1, B]   = toPartial0
      override def fromPartial: B => Either[E2, A] = fromPartial0
    }

  def unapply[A, B, E1, E2](self: PartialEquivalence[A, B, E1, E2]): Some[(A => Either[E1, B], B => Either[E2, A])] =
    Some((self.toPartial, self.fromPartial))

  def identity[A]: Equivalence[A, A] = Equivalence.identity[A]

}
