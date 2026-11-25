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

/**
 * An `Equivalence[A, B]` defines an equivalence between two types `A` and `B`.
 * These types represent different ways to store the same information.
 *
 * For example, a `List[Byte]` is equivalent to a `Vector[Byte]`. Similarly, a
 * `List[Char]` is equivalent to a `String`.
 *
 * Equivalences are symmetrical. So if `A` is equivalent to `B`, then `B` is
 * equivalent to `A`.
 *
 * A lawful equivalence round-trips in both directions: `from(to(a))` equals
 * `a` and `to(from(b))` equals `b`. Constructing one does not check this;
 * `zio.prelude.laws.EquivalenceLaws` can test it.
 *
 * {{{
 * import zio.Chunk
 *
 * val listChunk: Equivalence[List[Int], Chunk[Int]] =
 *   Equivalence(Chunk.fromIterable(_), _.toList)
 * val vectorList: Equivalence[Vector[Int], List[Int]] =
 *   Equivalence(_.toList, _.toVector)
 *
 * val vectorChunk: Equivalence[Vector[Int], Chunk[Int]] = vectorList >>> listChunk
 *
 * val chunk: Chunk[Int] = vectorChunk.to(Vector(1, 2))
 * // Chunk(1,2)
 * val vector: Vector[Int] = vectorChunk.from(Chunk(3))
 * // Vector(3)
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/functional-data-types/equivalence/ Equivalence]]
 */
trait Equivalence[A, B] extends PartialEquivalence[A, B, Nothing, Nothing] { self =>

  /**
   * A function that converts an `A` into a `B`.
   */
  def to: A => B

  /**
   * A function that converts a `B` into an `A`.
   */
  def from: B => A

  /**
   * Returns `to` as a function that always succeeds, wrapping its result in
   * `Right`.
   */
  final def toPartial: A => Either[Nothing, B] = (a: A) => Right(to(a))

  /**
   * Returns `from` as a function that always succeeds, wrapping its result in
   * `Right`.
   */
  final def fromPartial: B => Either[Nothing, A] = (b: B) => Right(from(b))

  /**
   * Composes this equivalence with the specified equivalence: the result
   * converts an `A` to a `B` with this equivalence, then the `B` to a `C` with
   * `that`, and converts back in the reverse order.
   */
  def >>>[C](that: Equivalence[B, C]): Equivalence[A, C] = self andThen that

  /**
   * A named method for `>>>`.
   */
  def andThen[C](that: Equivalence[B, C]): Equivalence[A, C] =
    Equivalence(self.to andThen that.to, self.from compose that.from)

  /**
   * Composes the specified equivalence with this one: the result converts a
   * `C` to an `A` with `that`, then the `A` to a `B` with this equivalence.
   * `a compose b` is `b andThen a`.
   */
  def compose[C](that: Equivalence[C, A]): Equivalence[C, B] = that andThen self

  /**
   * Flips this equivalence around, so `to` and `from` swap places.
   */
  override def flip: Equivalence[B, A] = Equivalence(from, to)

}

/**
 * Constructors of `Equivalence`, and equivalences between tuples and
 * `Either`s that hold the same information.
 */
object Equivalence {

  /**
   * Constructs an `Equivalence` from a function that converts an `A` into a
   * `B` and a function that converts a `B` into an `A`.
   */
  def apply[A, B](to0: A => B, from0: B => A): Equivalence[A, B] = new Equivalence[A, B] {
    override def to: A => B   = to0
    override def from: B => A = from0
  }

  /**
   * Constructs an `Equivalence` from two conversions that return `Either`
   * values whose error types are `Nothing`, so they always succeed. `ev1` and
   * `ev2` are the evidence that the error types are `Nothing`.
   */
  def fromPartial[A, B, E1, E2](
    toPartial0: A => Either[E1, B],
    fromPartial0: B => Either[E2, A]
  )(ev1: E1 <:< Nothing, ev2: E2 <:< Nothing): Equivalence[A, B] =
    Equivalence(
      toPartial0.andThen { case Left(value) => ev1(value); case Right(value) => value },
      fromPartial0.andThen { case Left(value) => ev2(value); case Right(value) => value }
    )

  /**
   * Extracts the `to` and `from` functions of an equivalence, for pattern
   * matching.
   */
  def unapply[A, B](self: Equivalence[A, B]): Some[(A => B, B => A)] =
    Some((self.to, self.from))

  /**
   * Constructs the identity equivalence, which just says that any type is
   * equivalent to itself and is trivially true.
   */
  def identity[A]: Equivalence[A, A] = Equivalence(Predef.identity[A], Predef.identity[A])

  /**
   * Constructs an equivalence between a right-associated nested tuple, and a
   * left-associated nested tuple.
   */
  def tuple[A, B, C]: Equivalence[(A, (B, C)), ((A, B), C)] =
    Equivalence(
      { case (a, (b, c)) =>
        ((a, b), c)
      },
      { case ((a, b), c) =>
        (a, (b, c))
      }
    )

  /**
   * Constructs an equivalence between a tuple and the same tuple with its
   * elements swapped.
   */
  def tupleFlip[A, B]: Equivalence[(A, B), (B, A)] =
    Equivalence(
      { case (a, b) =>
        (b, a)
      },
      { case (b, a) =>
        (a, b)
      }
    )

  /**
   * Constructs an equivalence between a pair whose second element is of type
   * `Any` and its first element. `to` drops the second element, and `from`
   * puts `()` in its place, so the round trip from a pair holds only when
   * values of type `Any` are treated as equal.
   */
  def tupleAny[A]: Equivalence[(A, Any), A] =
    Equivalence(_._1, a => (a, ()))

  /**
   * Constructs an equivalence between a right-associated nested either, and a
   * left-associated nested either.
   */
  def either[A, B, C]: Equivalence[Either[A, Either[B, C]], Either[Either[A, B], C]] =
    Equivalence(
      {
        case Left(a)         => Left(Left(a))
        case Right(Left(b))  => Left(Right(b))
        case Right(Right(c)) => Right(c)
      },
      {
        case Left(Left(a))  => Left(a)
        case Left(Right(b)) => Right(Left(b))
        case Right(c)       => Right(Right(c))
      }
    )

  /**
   * Constructs an equivalence between an `Either` and the same `Either` with
   * `Left` and `Right` swapped.
   */
  def eitherFlip[A, B]: Equivalence[Either[A, B], Either[B, A]] =
    Equivalence(
      {
        case Left(a)  => Right(a)
        case Right(b) => Left(b)
      },
      {
        case Right(a) => Left(a)
        case Left(b)  => Right(b)
      }
    )

  /**
   * Constructs an equivalence between an `Either[A, Nothing]`, which is always
   * a `Left`, and the value inside it.
   */
  def eitherNothing[A]: Equivalence[Either[A, Nothing], A] =
    Equivalence(
      {
        case Left(a)        => a
        case Right(nothing) => nothing
      },
      a => Left(a)
    )

}
