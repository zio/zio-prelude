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
   * Constructs a `Commutative` instance from a commutative binary operator.
   */
  def make[A](f: (A, A) => A): Commutative[A] =
    (l, r) => f(l, r)

  /**
   * Constructs an `Commutative` instance from an associative instance.
   */
  def makeFrom[A](associative: Associative[A]): Commutative[A] =
    make((l, r) => associative.combine(l, r))

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
    makeFrom(Associative.MapIdentity)

  /**
   * Derives a `Commutative[Option[A]]` given a `Commutative[A]`
   */
  implicit def OptionCommutative[A: Commutative]: Commutative[Option[A]] =
    makeFrom(Associative.OptionIdentity)

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple2Commutative[A: Commutative, B: Commutative]: Commutative[(A, B)] =
    makeFrom(Associative.Tuple2Associative)

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple3Commutative[A: Commutative, B: Commutative, C: Commutative]: Commutative[(A, B, C)] =
    makeFrom(Associative.Tuple3Associative)

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple4Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative]
    : Commutative[(A, B, C, D)] =
    makeFrom(Associative.Tuple4Associative)

  /**
   * Derives a `Commutative` for a product type given a `Commutative` for each
   * element of the product type.
   */
  implicit def Tuple5Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative, E: Commutative]
    : Commutative[(A, B, C, D, E)] =
    makeFrom(Associative.Tuple5Associative)

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
    makeFrom(Associative.Tuple6Associative)

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
    makeFrom(Associative.Tuple7Associative)

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
    makeFrom(Associative.Tuple8Associative)

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
    makeFrom(Associative.Tuple9Associative)

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
    makeFrom(Associative.Tuple10Associative)

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
    makeFrom(Associative.Tuple11Associative)

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
    makeFrom(Associative.Tuple12Associative)

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
    makeFrom(Associative.Tuple13Associative)

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
    makeFrom(Associative.Tuple14Associative)

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
    makeFrom(Associative.Tuple15Associative)

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
    makeFrom(Associative.Tuple16Associative)

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
    makeFrom(Associative.Tuple17Associative)

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
    makeFrom(Associative.Tuple18Associative)

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
    makeFrom(Associative.Tuple19Associative)

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
    makeFrom(Associative.Tuple20Associative)

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
    makeFrom(Associative.Tuple21Associative)

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
    makeFrom(Associative.Tuple22Associative)
}
