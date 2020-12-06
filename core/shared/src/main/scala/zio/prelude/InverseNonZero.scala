package zio.prelude

import zio.prelude.coherent.EqualInverseNonZero
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `InverseNonZero` type class describes an associative binary operator for a
 * type `A` that has an identity element and an inverse binary operator.
 * Combining any value with itself with the inverse operator must return the
 * identity element. The exception may be the `zero` element, where the result is not defined.
 * For example, for multiplication of reals, one is an identity
 * element and division is an inverse operation, because dividing any
 * value by itself always returns one.
 *
 * Because `InverseNonZero` defines a binary rather than a unary operator it can be
 * used to describe inverse operations for types that do not have inverse
 * values. For example, the natural numbers do not have inverses because the
 * set of natural numbers does not include negative numbers. But we can still
 * define a subtraction operation that is the inverse of addition for the
 * natural numbers, since subtracting a number from itself always returns
 * zero.
 */
trait InverseNonZero[A] extends Identity[A] {
  def inverse(l: => A, r: => A): A
}

object InverseNonZero extends Lawful[EqualInverseNonZero] {

  /**
   * The inverse law states that for some binary operator `*`, for all
   * values `a`, with the exception of the `zero` element, the following must hold:
   *
   * {{{
   * a * a === identity
   * }}}
   */
  val inverseLaw: Laws[EqualInverseNonZero] =
    new Laws.Law1[EqualInverseNonZero]("rightInverseNonZeroLaw") {
      def apply[A](a: A)(implicit I: EqualInverseNonZero[A]): TestResult =
        I.inverse(a, a) <-> I.identity
    }

  /**
   * The set of all laws that instances of `InverseNonZero` must satisfy.
   */
  val laws: Laws[EqualInverseNonZero] =
    inverseLaw + Identity.laws

  /**
   * Summons an implicit `InverseNonZero[A]`.
   */
  def apply[A](implicit InverseNonZero: InverseNonZero[A]): InverseNonZero[A] = InverseNonZero

  /**
   * Constructs an `InverseNonZero` instance from an associative binary operator, an
   * identity element, and an inverse binary operator.
   */
  def make[A](identity0: A, op: (A, A) => A, inv: (A, A) => A): InverseNonZero[A] =
    new InverseNonZero[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
      def inverse(l: => A, r: => A): A = inv(l, r)
    }

  /**
   * Constructs an `InverseNonZero` instance from an identity instance and
   * an inverse function.
   */
  def makeFrom[A](identity: Identity[A], inverse: (A, A) => A): InverseNonZero[A] =
    make(identity.identity, (l, r) => identity.combine(l, r), inverse)

  /**
   * Derives an `InverseNonZero[F[A]]` given a `Derive[F, InverseNonZero]` and an
   * `InverseNonZero[A]`.
   */
  implicit def DeriveInverseNonZero[F[_], A](implicit
    derive: Derive[F, InverseNonZero],
    inverse: InverseNonZero[A]
  ): InverseNonZero[F[A]] =
    derive.derive(inverse)

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple2InverseNonZero[A: InverseNonZero, B: InverseNonZero]: InverseNonZero[(A, B)] =
    makeFrom(
      Identity.Tuple2Identity,
      { case ((a1, b1), (a2, b2)) =>
        (a1 ~~ a2, b1 ~~ b2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple3InverseNonZero[A: InverseNonZero, B: InverseNonZero, C: InverseNonZero]
    : InverseNonZero[(A, B, C)] =
    makeFrom(
      Identity.Tuple3Identity,
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple4InverseNonZero[A: InverseNonZero, B: InverseNonZero, C: InverseNonZero, D: InverseNonZero]
    : InverseNonZero[(A, B, C, D)] =
    makeFrom(
      Identity.Tuple4Identity,
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple5InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E)] =
    makeFrom(
      Identity.Tuple5Identity,
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple6InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F)] =
    makeFrom(
      Identity.Tuple6Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1),
              (a2, b2, c2, d2, e2, f2)
            ) =>
          (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2, f1 ~~ f2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple7InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G)] =
    makeFrom(
      Identity.Tuple7Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1),
              (a2, b2, c2, d2, e2, f2, g2)
            ) =>
          (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2, f1 ~~ f2, g1 ~~ g2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple8InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      Identity.Tuple8Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1),
              (a2, b2, c2, d2, e2, f2, g2, h2)
            ) =>
          (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2, f1 ~~ f2, g1 ~~ g2, h1 ~~ h2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple9InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      Identity.Tuple9Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2)
            ) =>
          (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2, f1 ~~ f2, g1 ~~ g2, h1 ~~ h2, i1 ~~ i2)
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple10InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(
      Identity.Tuple10Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple11InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(
      Identity.Tuple11Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple12InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(
      Identity.Tuple12Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple13InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(
      Identity.Tuple13Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple14InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(
      Identity.Tuple14Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple15InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(
      Identity.Tuple15Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple16InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(
      Identity.Tuple16Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple17InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(
      Identity.Tuple17Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple18InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero,
    R: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(
      Identity.Tuple18Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2,
            r1 ~~ r2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple19InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero,
    R: InverseNonZero,
    S: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(
      Identity.Tuple19Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2,
            r1 ~~ r2,
            s1 ~~ s2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple20InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero,
    R: InverseNonZero,
    S: InverseNonZero,
    T: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(
      Identity.Tuple20Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2,
            r1 ~~ r2,
            s1 ~~ s2,
            t1 ~~ t2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple21InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero,
    R: InverseNonZero,
    S: InverseNonZero,
    T: InverseNonZero,
    U: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(
      Identity.Tuple21Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2,
            r1 ~~ r2,
            s1 ~~ s2,
            t1 ~~ t2,
            u1 ~~ u2
          )
      }
    )

  /**
   * Derives an `InverseNonZero` for a product type given an `InverseNonZero` for
   * each element of the product type.
   */
  implicit def Tuple22InverseNonZero[
    A: InverseNonZero,
    B: InverseNonZero,
    C: InverseNonZero,
    D: InverseNonZero,
    E: InverseNonZero,
    F: InverseNonZero,
    G: InverseNonZero,
    H: InverseNonZero,
    I: InverseNonZero,
    J: InverseNonZero,
    K: InverseNonZero,
    L: InverseNonZero,
    M: InverseNonZero,
    N: InverseNonZero,
    O: InverseNonZero,
    P: InverseNonZero,
    Q: InverseNonZero,
    R: InverseNonZero,
    S: InverseNonZero,
    T: InverseNonZero,
    U: InverseNonZero,
    V: InverseNonZero
  ]: InverseNonZero[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(
      Identity.Tuple22Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (
            a1 ~~ a2,
            b1 ~~ b2,
            c1 ~~ c2,
            d1 ~~ d2,
            e1 ~~ e2,
            f1 ~~ f2,
            g1 ~~ g2,
            h1 ~~ h2,
            i1 ~~ i2,
            j1 ~~ j2,
            k1 ~~ k2,
            l1 ~~ l2,
            m1 ~~ m2,
            n1 ~~ n2,
            o1 ~~ o2,
            p1 ~~ p2,
            q1 ~~ q2,
            r1 ~~ r2,
            s1 ~~ s2,
            t1 ~~ t2,
            u1 ~~ u2,
            v1 ~~ v2
          )
      }
    )

}

trait InverseNonZeroSyntax {

  /**
   * Provides infix syntax for combining two values with an inverse
   * operation.
   */
  implicit class InverseNonZeroOps[A](l: A) {

    /**
     * A symbolic alias for `inverse`.
     */
    def ~~(r: => A)(implicit inverse: InverseNonZero[A]): A =
      inverse.inverse(l, r)

    /**
     * InverseNonZeros this value with the specified value
     */
    def inverse(r: => A)(implicit inverse: InverseNonZero[A]): A =
      inverse.inverse(l, r)
  }
}
