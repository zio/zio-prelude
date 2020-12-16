package zio.prelude

import zio.prelude.coherent.EqualPartialInverse
import zio.test.laws.{ Lawful, Laws }
import zio.test.{ TestResult, assertCompletes }

import scala.annotation.tailrec

/**
 * The `PartialInverse` type class describes an associative binary operator for a
 * type `A` that has an identity element and an inverse binary operator.
 * Combining any value with itself with the inverse operator must return the
 * identity element. There may be an exception, where the result is not defined.
 * For example, for multiplication of reals, one is an identity
 * element and division is an inverse operation, because dividing any
 * value by itself always returns one.
 *
 * Because `PartialInverse` defines a binary rather than a unary operator it can be
 * used to describe inverse operations for types that do not have inverse
 * values. For example, the natural numbers do not have inverses because the
 * set of natural numbers does not include negative numbers. But we can still
 * define a subtraction operation that is the inverse of addition for the
 * natural numbers, since subtracting a number from itself always returns
 * zero.
 */
trait PartialInverse[A] extends Identity[A] {

  def inverseOption(l: => A, r: => A): Option[A]

  override def multiplyOption(n: Int)(a: A): Option[A] = {
    @tailrec
    def multiplyHelper(res: Option[A], n: Int): Option[A] = res match {
      case Some(res) =>
        if (n == 0) Some(res)
        else if (n > 0) multiplyHelper(Some(combine(a, res)), n - 1)
        else multiplyHelper(inverseOption(res, a), n + 1)
      case _         => res
    }

    multiplyHelper(Some(identity), n)
  }
}

object PartialInverse extends Lawful[EqualPartialInverse] {

  /**
   * The partial inverse law states that for some binary operator `*`,
   * for all values `a`, if the operation is defined, the following must hold:
   *
   * {{{
   * a * a === identity
   * }}}
   */
  val partialInverseLaw: Laws[EqualPartialInverse] =
    new Laws.Law1[EqualPartialInverse]("rightPartialInverseLaw") {
      def apply[A](a: A)(implicit I: EqualPartialInverse[A]): TestResult =
        I.inverseOption(a, a) match {
          case Some(a) => a <-> I.identity
          case None    => assertCompletes
        }
    }

  /**
   * The set of all laws that instances of `PartialInverse` must satisfy.
   */
  val laws: Laws[EqualPartialInverse] =
    partialInverseLaw + Identity.laws

  /**
   * Summons an implicit `PartialInverse[A]`.
   */
  def apply[A](implicit PartialInverse: PartialInverse[A]): PartialInverse[A] = PartialInverse

  /**
   * Constructs an `PartialInverse` instance from an associative binary operator, an
   * identity element, and an inverse binary operator.
   */
  def make[A](identity0: A, op: (A, A) => A, inv: (A, A) => Option[A]): PartialInverse[A] =
    new PartialInverse[A] {
      def identity: A                                = identity0
      def combine(l: => A, r: => A): A               = op(l, r)
      def inverseOption(l: => A, r: => A): Option[A] = inv(l, r)
    }

  /**
   * Constructs an `PartialInverse` instance from an identity instance and
   * an inverse function.
   */
  def makeFrom[A](identity: Identity[A], inverse: (A, A) => Option[A]): PartialInverse[A] =
    make(identity.identity, (l, r) => identity.combine(l, r), inverse)

  /**
   * Derives an `PartialInverse[F[A]]` given a `Derive[F, PartialInverse]` and an
   * `PartialInverse[A]`.
   */
  implicit def DerivePartialInverse[F[_], A](implicit
    derive: Derive[F, PartialInverse],
    inverse: PartialInverse[A]
  ): PartialInverse[F[A]] =
    derive.derive(inverse)

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple2PartialInverse[A: PartialInverse, B: PartialInverse]: PartialInverse[(A, B)] =
    makeFrom(
      Identity.Tuple2Identity,
      { case ((a1, b1), (a2, b2)) =>
        (a1 ~?~ a2, b1 ~?~ b2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple3PartialInverse[A: PartialInverse, B: PartialInverse, C: PartialInverse]
    : PartialInverse[(A, B, C)] =
    makeFrom(
      Identity.Tuple3Identity,
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple4PartialInverse[A: PartialInverse, B: PartialInverse, C: PartialInverse, D: PartialInverse]
    : PartialInverse[(A, B, C, D)] =
    makeFrom(
      Identity.Tuple4Identity,
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple5PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse
  ]: PartialInverse[(A, B, C, D, E)] =
    makeFrom(
      Identity.Tuple5Identity,
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2, e1 ~?~ e2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple6PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F)] =
    makeFrom(
      Identity.Tuple6Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1),
              (a2, b2, c2, d2, e2, f2)
            ) =>
          (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2, e1 ~?~ e2, f1 ~?~ f2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple7PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G)] =
    makeFrom(
      Identity.Tuple7Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1),
              (a2, b2, c2, d2, e2, f2, g2)
            ) =>
          (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2, e1 ~?~ e2, f1 ~?~ f2, g1 ~?~ g2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple8PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      Identity.Tuple8Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1),
              (a2, b2, c2, d2, e2, f2, g2, h2)
            ) =>
          (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2, e1 ~?~ e2, f1 ~?~ f2, g1 ~?~ g2, h1 ~?~ h2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple9PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      Identity.Tuple9Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2)
            ) =>
          (a1 ~?~ a2, b1 ~?~ b2, c1 ~?~ c2, d1 ~?~ d2, e1 ~?~ e2, f1 ~?~ f2, g1 ~?~ g2, h1 ~?~ h2, i1 ~?~ i2).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple10PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(
      Identity.Tuple10Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple11PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(
      Identity.Tuple11Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple12PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(
      Identity.Tuple12Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple13PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(
      Identity.Tuple13Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple14PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(
      Identity.Tuple14Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple15PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(
      Identity.Tuple15Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple16PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(
      Identity.Tuple16Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple17PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(
      Identity.Tuple17Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple18PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse,
    R: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(
      Identity.Tuple18Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2,
            r1 ~?~ r2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple19PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse,
    R: PartialInverse,
    S: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(
      Identity.Tuple19Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2,
            r1 ~?~ r2,
            s1 ~?~ s2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple20PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse,
    R: PartialInverse,
    S: PartialInverse,
    T: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(
      Identity.Tuple20Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2,
            r1 ~?~ r2,
            s1 ~?~ s2,
            t1 ~?~ t2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple21PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse,
    R: PartialInverse,
    S: PartialInverse,
    T: PartialInverse,
    U: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(
      Identity.Tuple21Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2,
            r1 ~?~ r2,
            s1 ~?~ s2,
            t1 ~?~ t2,
            u1 ~?~ u2
          ).tupleN
      }
    )

  /**
   * Derives an `PartialInverse` for a product type given an `PartialInverse` for
   * each element of the product type.
   */
  implicit def Tuple22PartialInverse[
    A: PartialInverse,
    B: PartialInverse,
    C: PartialInverse,
    D: PartialInverse,
    E: PartialInverse,
    F: PartialInverse,
    G: PartialInverse,
    H: PartialInverse,
    I: PartialInverse,
    J: PartialInverse,
    K: PartialInverse,
    L: PartialInverse,
    M: PartialInverse,
    N: PartialInverse,
    O: PartialInverse,
    P: PartialInverse,
    Q: PartialInverse,
    R: PartialInverse,
    S: PartialInverse,
    T: PartialInverse,
    U: PartialInverse,
    V: PartialInverse
  ]: PartialInverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(
      Identity.Tuple22Identity,
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (
            a1 ~?~ a2,
            b1 ~?~ b2,
            c1 ~?~ c2,
            d1 ~?~ d2,
            e1 ~?~ e2,
            f1 ~?~ f2,
            g1 ~?~ g2,
            h1 ~?~ h2,
            i1 ~?~ i2,
            j1 ~?~ j2,
            k1 ~?~ k2,
            l1 ~?~ l2,
            m1 ~?~ m2,
            n1 ~?~ n2,
            o1 ~?~ o2,
            p1 ~?~ p2,
            q1 ~?~ q2,
            r1 ~?~ r2,
            s1 ~?~ s2,
            t1 ~?~ t2,
            u1 ~?~ u2,
            v1 ~?~ v2
          ).tupleN
      }
    )

}

trait PartialInverseSyntax {

  /**
   * Provides infix syntax for combining two values with an inverse
   * operation.
   */
  implicit class PartialInverseOps[A](l: A) {

    /**
     * A symbolic alias for `inverse`.
     */
    def ~?~(r: => A)(implicit inverse: PartialInverse[A]): Option[A] =
      inverse.inverseOption(l, r)

    /**
     * PartialInverses this value with the specified value
     */
    def inverseOption(r: => A)(implicit inverse: PartialInverse[A]): Option[A] =
      inverse.inverseOption(l, r)

  }
}
