package zio.prelude

import zio.prelude.coherent.CommutativeEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Commutative` type class describes a binary operator for a type `A` that
 * is both associative and commutative. This means that `a1 <> a2` is equal to
 * `a2 <> a1` for all values `a1` and `a2`. Examples of commutative operations
 * include addition for integers, but not concatenation for strings.
 *
 * Commutative operators are useful because combining values with a commutative
 * operation results in the same value regardless of the order in which values
 * are combined, allowing us to combine values in the order that is most
 * efficient and allowing us to return determinate values even when the order
 * of original values is indeterminate.
 */
trait Commutative[A] extends Associative[A] { self =>

  /**
   * Returns a new `Commutative` instance that describes the same binary
   * operator but applied in reverse order. Since the operation is commutative
   * this instance is guaranteed to return the same results as the original
   * instance but one order of combination or the other may be more efficient
   * in certain cases.
   */
  final def commute: Commutative[A] = Commutative((l, r) => self.combine(r, l))
}

object Commutative extends Lawful[CommutativeEqual] {

  /**
   * The commutative law states that for some binary operator `*`, for all
   * values `a1` and `a2`, the following must hold:
   *
   * {{{
   * a1 * a2 === a2 * a1
   * }}}
   */
  val commutativeLaw: Laws[CommutativeEqual] =
    new Laws.Law2[CommutativeEqual]("commutativeLaw") {
      def apply[A: CommutativeEqual](a1: A, a2: A): TestResult =
        (a1 <> a2) <-> (a2 <> a1)
    }

  /**
   * The set of all laws that instances of `Commutative` must satisfy.
   */
  val laws: Laws[CommutativeEqual] =
    commutativeLaw

  /**
   * Summons an implicit `Commutative[A]`.
   */
  def apply[A](implicit commutative: Commutative[A]): Commutative[A] = commutative

  /**
   * Constructs a `Commutative` instance from a function.
   */
  def make[A](f: (A, A) => A): Commutative[A] =
    (l, r) => f(l, r)

  /**
   * Derives a `Commutative[F[A]]` given a `Derive[F, Commutative]` and a
   * `Commutative[A]`.
   */
  implicit def DeriveCommutative[F[_], A](
    implicit derive: Derive[F, Commutative],
    commutative: Commutative[A]
  ): Commutative[F[A]] =
    derive.derive(commutative)

  /**
   * Derives a `Commutative[Either[E, A]]` given a `Commutative[E]` and a
   * `Commutative[A]`.
   */
  implicit def EitherCommutative[E: Commutative, A: Commutative]: Commutative[Either[E, A]] =
    new Commutative[Either[E, A]] {
      def combine(l: => Either[E, A], r: => Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Right(l), Right(r)) => Right(l <> r)
          case (Left(l), Right(_))  => Left(l)
          case (Right(_), Left(r))  => Left(r)
          case (Left(l), Left(r))   => Left(l <> r)
        }
    }

  /**
   * Derives a `Commutative[Map[K, V]]` given a `Commutative[V]`.
   */
  implicit def MapCommutative[K, V: Commutative]: Commutative[Map[K, V]] =
    new Commutative[Map[K, V]] {

      def combine(l: => Map[K, V], r: => Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  /**
   * Derives a `Commutative[Option[A]]` given a `Commutative[A]`
   */
  implicit def OptionCommutative[A: Commutative]: Commutative[Option[A]] =
    new Commutative[Option[A]] {
      def combine(l: => Option[A], r: => Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple2Commutative[A: Commutative, B: Commutative]: Commutative[(A, B)] =
    new Commutative[(A, B)] {
      def combine(l: => (A, B), r: => (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple3Commutative[A: Commutative, B: Commutative, C: Commutative]: Commutative[(A, B, C)] =
    new Commutative[(A, B, C)] {
      def combine(l: => (A, B, C), r: => (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple4Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative]
    : Commutative[(A, B, C, D)] =
    new Commutative[(A, B, C, D)] {
      def combine(l: => (A, B, C, D), r: => (A, B, C, D)): (A, B, C, D) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple5Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative, E: Commutative]
    : Commutative[(A, B, C, D, E)] =
    new Commutative[(A, B, C, D, E)] {
      def combine(l: => (A, B, C, D, E), r: => (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple6Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative
  ]: Commutative[(A, B, C, D, E, F)] =
    new Commutative[(A, B, C, D, E, F)] {
      def combine(l: => (A, B, C, D, E, F), r: => (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple7Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative
  ]: Commutative[(A, B, C, D, E, F, G)] =
    new Commutative[(A, B, C, D, E, F, G)] {
      def combine(l: => (A, B, C, D, E, F, G), r: => (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple8Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H)] =
    new Commutative[(A, B, C, D, E, F, G, H)] {
      def combine(l: => (A, B, C, D, E, F, G, H), r: => (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7, l._8 <> r._8)
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple9Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I)] =
    new Commutative[(A, B, C, D, E, F, G, H, I)] {
      def combine(l: => (A, B, C, D, E, F, G, H, I), r: => (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple10Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J),
        r: => (A, B, C, D, E, F, G, H, I, J)
      ): (A, B, C, D, E, F, G, H, I, J) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple11Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K),
        r: => (A, B, C, D, E, F, G, H, I, J, K)
      ): (A, B, C, D, E, F, G, H, I, J, K) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple12Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L)
      ): (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple13Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple14Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple15Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple16Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple17Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple18Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple19Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple20Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple21Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative,
    U: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21
        )
    }

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple22Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative,
    U: Commutative,
    V: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21,
          l._22 <> r._22
        )
    }
}
