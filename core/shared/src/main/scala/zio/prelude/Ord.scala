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

import zio.prelude.Equal._
import zio.prelude.coherent.{HashOrd, HashPartialOrd}
import zio.{Chunk, NonEmptyChunk}

import scala.annotation.{implicitNotFound, tailrec}
import scala.{math => sm}

/**
 * `Ord[A]` provides implicit evidence that values of type `A` have a total
 * ordering.
 */
@implicitNotFound("No implicit Ord defined for ${A}.")
trait Ord[-A] extends PartialOrd[A] { self =>

  /**
   * Returns the result of comparing two values of type `A`.
   */
  final override def compare(l: A, r: A): Ordering =
    if (Equal.refEq(l, r)) Ordering.Equals else checkCompare(l, r)

  /**
   * Returns the result of comparing two values of type `A`.
   */
  protected def checkCompare(l: A, r: A): Ordering

  override protected def checkEqual(l: A, r: A): Boolean =
    compare(l, r).isEqual

  /**
   * Constructs an `Ord[(A, B)]` given an `Ord[A]` and `Ord[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  final def both[B](that: => Ord[B]): Ord[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  final def bothWith[B, C](that: => Ord[B])(f: C => (A, B)): Ord[C] =
    Ord.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.compare(a1, a2) <> that.compare(b1, b2)
      }
    }

  /**
   * Constructs an `Ord[B]` given an `Ord[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  override def contramap[B](f: B => A): Ord[B] =
    Ord.make((b1, b2) => compare(f(b1), f(b2)))

  /**
   * Constructs an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  final def either[B](that: => Ord[B]): Ord[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  final def eitherWith[B, C](that: => Ord[B])(f: C => Either[A, B]): Ord[C] =
    Ord.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.compare(a1, a2)
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => that.compare(b1, b2)
      }
    }

  /**
   * Constructs a new `Ord[A]` by mapping the result of this ordering using the
   * specified function.
   */
  final def mapOrdering(f: Ordering => Ordering): Ord[A] =
    Ord.make((l, r) => f(compare(l, r)))

  /**
   * Returns the maximum of the left value and the right value.
   */
  final def max[A1 <: A](l: A1, r: A1): A1 =
    if (greaterOrEqual(l, r)) l else r

  /**
   * Returns the minimum of the left value and the right value.
   */
  final def min[A1 <: A](l: A1, r: A1): A1 =
    if (lessOrEqual(l, r)) l else r

  /**
   * Returns a new ordering that is the reverse of this one.
   */
  final def reverse: Ord[A] =
    mapOrdering(_.opposite)

  override def toScala[A1 <: A]: sm.Ordering[A1] =
    self.compare(_, _) match {
      case Ordering.LessThan    => -1
      case Ordering.Equals      => 0
      case Ordering.GreaterThan => 1
    }
}

object Ord {

  def fromScala[A](implicit ordering: sm.Ordering[A]): Ord[A] =
    (l: A, r: A) => Ordering.fromCompare(ordering.compare(l, r))

  /**
   * The `Contravariant` instance for `Ord`.
   */
  implicit val OrdContravariant: Contravariant[Ord] =
    new Contravariant[Ord] {
      def contramap[A, B](f: B => A): Ord[A] => Ord[B] =
        _.contramap(f)
    }

  /**
   * The `IdentityBoth` instance for `Ord`.
   */
  implicit val OrdIdentityBoth: IdentityBoth[Ord] =
    new IdentityBoth[Ord] {
      val any: Ord[Any]                                         =
        AnyHashOrd
      def both[A, B](fa: => Ord[A], fb: => Ord[B]): Ord[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `IdentityEither` instance for `Ord`.
   */
  implicit val OrdIdentityEither: IdentityEither[Ord] =
    new IdentityEither[Ord] {
      def either[A, B](fa: => Ord[A], fb: => Ord[B]): Ord[Either[A, B]] =
        fa.either(fb)
      val none: Ord[Nothing]                                            =
        NothingHashOrd
    }

  /**
   * Summons an implicit `Ord[A]`.
   */
  def apply[A](implicit ord: Ord[A]): Ord[A] =
    ord

  /**
   * Constructs an `Ord[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def make[A](ord: (A, A) => Ordering): Ord[A] = new Ord[A] {
    override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
  }

  /**
   * Constructs an instance from an `ord` function and a `equal0` function.
   * Since this takes a separate `equal0`, short-circuiting the equality check (failing fast) is possible.
   */
  def makeFrom[A](ord: (A, A) => Ordering, equal0: Equal[A]): Ord[A] =
    new Ord[A] {
      override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean    = equal0.equal(l, r)
    }

  /**
   * Constructs an `Ord[A]` from a [[scala.math.Ordering]].
   */
  def default[A](implicit ord: scala.math.Ordering[A]): Ord[A] =
    makeFrom((a1, a2) => Ordering.fromCompare(ord.compare(a1, a2)), Equal.fromScala(ord))

  /**
   * Derives an `Ord[Chunk[A]]` given an `Ord[A]`.
   */
  implicit def ChunkOrd[A: Ord]: Ord[Chunk[A]] =
    makeFrom(
      { (l, r) =>
        val j    = l.length
        val k    = r.length
        val OrdA = Ord[A]

        @tailrec
        def loop(i: Int): Ordering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else {
            val compare = OrdA.compare(l(i), r(i))
            if (compare.isEqual) loop(i + 1) else compare
          }

        loop(0)
      },
      PartialOrd.ChunkPartialOrd
    )

  /**
   * Derives an `Ord[F[A]]` given a `Derive[F, Ord]` and an `Ord[A]`.
   */
  implicit def DeriveOrd[F[_], A](implicit derive: Derive[F, Ord], ord: Ord[A]): Ord[F[A]] =
    derive.derive(ord)

  /**
   * Derives an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`.
   */
  implicit def EitherOrd[A: Ord, B: Ord]: Ord[Either[A, B]] =
    makeFrom(
      {
        case (Left(a1), Left(a2))   => a1 =?= a2
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => b1 =?= b2
      },
      PartialOrd.EitherPartialOrd
    )

  /**
   * Derives an `Ord[List[A]]` given an `Ord[A]`.
   */
  implicit def ListOrd[A: Ord]: Ord[List[A]] = {
    val OrdA = Ord[A]

    @tailrec
    def loop(left: List[A], right: List[A]): Ordering =
      (left, right) match {
        case (Nil, Nil)           => Ordering.Equals
        case (Nil, _)             => Ordering.LessThan
        case (_, Nil)             => Ordering.GreaterThan
        case (h1 :: t1, h2 :: t2) =>
          val compare = OrdA.compare(h1, h2)
          if (compare.isEqual) loop(t1, t2) else compare
      }

    makeFrom((l, r) => loop(l, r), PartialOrd.ListPartialOrd)
  }

  /**
   * Derives an `Ord[NonEmptyChunk[A]]` given an `Ord[A]`.
   */
  implicit def NonEmptyChunkOrd[A: Ord]: Ord[NonEmptyChunk[A]] =
    Ord[Chunk[A]].contramap(_.toChunk)

  /**
   * Derives an `Ord[Option[A]]` given an `Ord[A]`. `None` will be treated as
   * less than all other values.
   */
  implicit def OptionOrd[A: Ord]: Ord[Option[A]] =
    Ord[Unit].eitherWith(Ord[A]) {
      case None    => Left(())
      case Some(a) => Right(a)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple2Ord[A: Ord, B: Ord]: Ord[(A, B)] =
    makeFrom(
      { case ((a1, b1), (a2, b2)) =>
        (a1 =?= a2) <> (b1 =?= b2)
      },
      PartialOrd.Tuple2PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple3Ord[A: Ord, B: Ord, C: Ord]: Ord[(A, B, C)] =
    makeFrom(
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2)
      },
      PartialOrd.Tuple3PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple4Ord[A: Ord, B: Ord, C: Ord, D: Ord]: Ord[(A, B, C, D)] =
    makeFrom(
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2)
      },
      PartialOrd.Tuple4PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple5Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord]: Ord[(A, B, C, D, E)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2)
      },
      PartialOrd.Tuple5PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple6Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord]: Ord[(A, B, C, D, E, F)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2)
      },
      PartialOrd.Tuple6PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple7Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord]: Ord[(A, B, C, D, E, F, G)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2)
      },
      PartialOrd.Tuple7PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple8Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord]
    : Ord[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2)
      },
      PartialOrd.Tuple8PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple9Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord, I: Ord]
    : Ord[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2)
      },
      PartialOrd.Tuple9PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple10Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2)
      },
      PartialOrd.Tuple10PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple11Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2)
      },
      PartialOrd.Tuple11PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple12Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2)
      },
      PartialOrd.Tuple12PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple13Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2)
      },
      PartialOrd.Tuple13PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple14Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2)
      },
      PartialOrd.Tuple14PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple15Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2)
      },
      PartialOrd.Tuple15PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple16Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2)
      },
      PartialOrd.Tuple16PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple17Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2)
      },
      PartialOrd.Tuple17PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple18Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2)
      },
      PartialOrd.Tuple18PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple19Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2)
      },
      PartialOrd.Tuple19PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple20Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2)
      },
      PartialOrd.Tuple20PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple21Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord,
    U: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2)
      },
      PartialOrd.Tuple21PartialOrd
    )

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple22Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord,
    U: Ord,
    V: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2) <> (v1 =?= v2)
      },
      PartialOrd.Tuple22PartialOrd
    )

  /**
   * Derives an `Ord[Vector[A]]` given an `Ord[A]`.
   */
  implicit def VectorOrd[A: Ord]: Ord[Vector[A]] =
    makeFrom(
      { (l, r) =>
        val j    = l.length
        val k    = r.length
        val OrdA = Ord[A]

        @tailrec
        def loop(i: Int): Ordering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else {
            val compare = OrdA.compare(l(i), r(i))
            if (compare.isEqual) loop(i + 1) else compare
          }

        loop(0)
      },
      PartialOrd.VectorPartialOrd
    )
}

trait OrdSyntax {

  /**
   * Provides infix syntax for comparing two values with a total ordering.
   */
  implicit class OrdOps[A](val l: A) {

    /**
     * Returns the result of comparing this value with the specified value.
     */
    def =?=[A1 >: A](r: A1)(implicit ord: Ord[A1]): Ordering = ord.compare(l, r)

    /**
     * Returns the maximum of this value and the specified value.
     */
    def max[A1 >: A](r: A1)(implicit ord: Ord[A1]): A1 =
      ord.max(l, r)

    /**
     * Returns the minimum of this value and the specified value.
     */
    def min[A1 >: A](r: A1)(implicit ord: Ord[A1]): A1 =
      ord.min(l, r)
  }
}

sealed trait Comparison extends Product with Serializable

object Comparison {

  sealed trait NotEqual extends Comparison

}

sealed trait PartialOrdering extends Product with Serializable { self =>

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>(that: => PartialOrdering): PartialOrdering =
    self orElse that

  /**
   * Returns whether this `Ordering` is `Ordering.Equals`.
   */
  final def isEqual: Boolean =
    self match {
      case Ordering.Equals => true
      case _               => false
    }

  /**
   * Returns whether this `Ordering` is `Ordering.GreaterThan`.
   */
  final def isGreaterThan: Boolean =
    self match {
      case Ordering.GreaterThan => true
      case _                    => false
    }

  /**
   * Returns whether this `Ordering` is `Ordering.LessThan`.
   */
  final def isLessThan: Boolean =
    self match {
      case Ordering.LessThan => true
      case _                 => false
    }

  /**
   * Returns this ordering, but if this ordering is equal returns the
   * specified ordering.
   */
  final def orElse(that: => PartialOrdering): PartialOrdering =
    self match {
      case Ordering.Equals => that
      case ordering        => ordering
    }

  def unify(that: PartialOrdering): PartialOrdering = (self, that) match {
    case (Ordering.LessThan, Ordering.LessThan)       => Ordering.LessThan
    case (Ordering.GreaterThan, Ordering.GreaterThan) => Ordering.GreaterThan
    case (Ordering.Equals, that)                      => that
    case (self, Ordering.Equals)                      => self
    case _                                            => PartialOrdering.Incomparable
  }
}

object PartialOrdering {

  case object Incomparable extends PartialOrdering with Comparison.NotEqual

  /**
   * `Hash` and `PartialOrd` instance for `PartialOrdering` values.
   */
  implicit val PartialOrderingHashPartialOrd: Hash[PartialOrdering] with PartialOrd[PartialOrdering] =
    HashPartialOrd.make(
      (x: PartialOrdering) => x.hashCode,
      (l: PartialOrdering, r: PartialOrdering) =>
        (l, r) match {
          case (l: Ordering, r: Ordering)   => Ordering.OrderingHashOrd.compare(l, r)
          case (Incomparable, Incomparable) => Ordering.Equals
          case _                            => Incomparable
        }
    )

  /**
   * `Idempotent`, `Identity` (and thus `Associative`) instance for `PartialOrdering` values.
   */
  implicit val PartialOrderingIdempotentIdentity: Idempotent[PartialOrdering] with Identity[PartialOrdering] =
    new Idempotent[PartialOrdering] with Identity[PartialOrdering] {
      override def combine(l: => PartialOrdering, r: => PartialOrdering): PartialOrdering = l <> r
      override def identity: PartialOrdering                                              = Ordering.Equals
    }

  /**
   * `Idempotent`, `Identity` (and thus `Associative`) instance for `PartialOrdering` values
   * that combines them for non-lexicographic purposes.
   */
  val PartialOrderingNonlexicographicCommutativeIdempotentIdentity
    : Commutative[PartialOrdering] with Idempotent[PartialOrdering] with Identity[PartialOrdering] =
    new Commutative[PartialOrdering] with Idempotent[PartialOrdering] with Identity[PartialOrdering] {
      override def combine(l: => PartialOrdering, r: => PartialOrdering): PartialOrdering = l.unify(r)
      override def identity: PartialOrdering                                              = Ordering.Equals
    }
}

/**
 * An `Ordering` is the result of comparing two values. The result may be
 * `LessThan`, `Equals`, or `GreaterThan`.
 */
sealed trait Ordering extends PartialOrdering { self =>

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>(that: => Ordering): Ordering =
    self orElse that

  /**
   * Converts this `Ordering` to an ordinal representation, with `0`
   * representing `LessThan`, `1` representing `Equals` and `2` representing
   * `GreaterThan`.
   */
  final def ordinal: Int =
    self match {
      case Ordering.LessThan    => 0
      case Ordering.Equals      => 1
      case Ordering.GreaterThan => 2
    }

  /**
   * Returns this ordering, but if this ordering is equal returns the
   * specified ordering.
   */
  final def orElse(that: => Ordering): Ordering =
    self match {
      case Ordering.Equals => that
      case ordering        => ordering
    }

  /**
   * Returns the opposite of this `Ordering`, with `LessThan` converted to
   * `GreaterThan` and `GreaterThan` converted to `LessThan`.
   */
  final def opposite: Ordering =
    self match {
      case Ordering.LessThan    => Ordering.GreaterThan
      case Ordering.Equals      => Ordering.Equals
      case Ordering.GreaterThan => Ordering.LessThan
    }
}

object Ordering {
  case object LessThan    extends Ordering with Comparison.NotEqual
  case object Equals      extends Ordering with Comparison
  case object GreaterThan extends Ordering with Comparison.NotEqual

  /**
   * Converts an integer result from [[scala.math.Ordering.compare]] or
   * [[java.lang.Comparable]] to a `Compare`.
   */
  def fromCompare(n: Int): Ordering =
    if (n < 0) LessThan
    else if (n > 0) GreaterThan
    else Equals

  /**
   * `Hash` and `Ord` instance for `Ordering` values.
   */
  implicit val OrderingHashOrd: Hash[Ordering] with Ord[Ordering] =
    HashOrd.make(
      (x: Ordering) => x.hashCode,
      (l: Ordering, r: Ordering) => Ord[Int].compare(l.ordinal, r.ordinal)
    )

  /**
   * `Idempotent`, `Identity` (and thus `Associative`) instance for `Ordering` values.
   */
  implicit val OrderingIdempotentIdentity: Idempotent[Ordering] with Identity[Ordering] =
    new Idempotent[Ordering] with Identity[Ordering] {
      override def combine(l: => Ordering, r: => Ordering): Ordering = l match {
        case Ordering.Equals => r
        case l               => l
      }

      override def identity: Ordering = Ordering.Equals
    }
}
