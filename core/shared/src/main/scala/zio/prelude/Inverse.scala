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

import scala.annotation.tailrec

/**
 * The `Inverse` type class describes an associative binary operator for a
 * type `A` that has an identity element and an inverse binary operator.
 * Combining any value with itself with the inverse operator must return the
 * identity element. For example, for integer addition zero is an identity
 * element and subtraction is an inverse operation, because subtracting any
 * value from itself always returns zero.
 *
 * Because `Inverse` defines a binary rather than a unary operator it can be
 * used to describe inverse operations for types that do not have inverse
 * values. For example, the natural numbers do not have inverses because the
 * set of natural numbers does not include negative numbers. But we can still
 * define a subtraction operation that is the inverse of addition for the
 * natural numbers, since subtracting a number from itself always returns
 * zero.
 */
trait Inverse[A] extends Identity[A] {
  def inverse(l: => A, r: => A): A

  def multiply(n: Int)(a: A): A = {
    @tailrec
    def multiplyHelper(res: A, n: Int): A =
      if (n == 0) res
      else if (n > 0) multiplyHelper(combine(a, res), n - 1)
      else multiplyHelper(inverse(res, a), n + 1)
    multiplyHelper(identity, n)
  }

  override def multiplyOption(n: Int)(a: A): Some[A] =
    Some(multiply(n)(a))
}

object Inverse {

  /**
   * Summons an implicit `Inverse[A]`.
   */
  def apply[A](implicit Inverse: Inverse[A]): Inverse[A] = Inverse

  /**
   * Constructs an `Inverse` instance from an associative binary operator, an
   * identity element, and an inverse binary operator.
   */
  def make[A](identity0: A, op: (A, A) => A, inv: (A, A) => A): Inverse[A] =
    new Inverse[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
      def inverse(l: => A, r: => A): A = inv(l, r)
    }

  /**
   * Constructs an `Inverse` instance from an identity instance and
   * an inverse function.
   */
  def makeFrom[A](identity: Identity[A], inverse: (A, A) => A): Inverse[A] =
    make(identity.identity, (l, r) => identity.combine(l, r), inverse)

  /**
   * Derives an `Inverse[F[A]]` given a `Derive[F, Inverse]` and an
   * `Inverse[A]`.
   */
  implicit def DeriveInverse[F[_], A](implicit derive: Derive[F, Inverse], inverse: Inverse[A]): Inverse[F[A]] =
    derive.derive(inverse)

  /**
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple2Inverse[A: Inverse, B: Inverse]: Inverse[(A, B)] =
    makeFrom(
      Identity.Tuple2Identity,
      { case ((a1, b1), (a2, b2)) =>
        (a1 ~~ a2, b1 ~~ b2)
      }
    )

  /**
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple3Inverse[A: Inverse, B: Inverse, C: Inverse]: Inverse[(A, B, C)] =
    makeFrom(
      Identity.Tuple3Identity,
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2)
      }
    )

  /**
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple4Inverse[A: Inverse, B: Inverse, C: Inverse, D: Inverse]: Inverse[(A, B, C, D)] =
    makeFrom(
      Identity.Tuple4Identity,
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2)
      }
    )

  /**
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple5Inverse[A: Inverse, B: Inverse, C: Inverse, D: Inverse, E: Inverse]: Inverse[(A, B, C, D, E)] =
    makeFrom(
      Identity.Tuple5Identity,
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 ~~ a2, b1 ~~ b2, c1 ~~ c2, d1 ~~ d2, e1 ~~ e2)
      }
    )

  /**
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple6Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse
  ]: Inverse[(A, B, C, D, E, F)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple7Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse
  ]: Inverse[(A, B, C, D, E, F, G)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple8Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple9Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple10Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple11Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple12Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple13Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple14Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple15Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple16Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple17Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple18Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse,
    R: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple19Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse,
    R: Inverse,
    S: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple20Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse,
    R: Inverse,
    S: Inverse,
    T: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple21Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse,
    R: Inverse,
    S: Inverse,
    T: Inverse,
    U: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
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
   * Derives an `Inverse` for a product type given an `Inverse` for
   * each element of the product type.
   */
  implicit def Tuple22Inverse[
    A: Inverse,
    B: Inverse,
    C: Inverse,
    D: Inverse,
    E: Inverse,
    F: Inverse,
    G: Inverse,
    H: Inverse,
    I: Inverse,
    J: Inverse,
    K: Inverse,
    L: Inverse,
    M: Inverse,
    N: Inverse,
    O: Inverse,
    P: Inverse,
    Q: Inverse,
    R: Inverse,
    S: Inverse,
    T: Inverse,
    U: Inverse,
    V: Inverse
  ]: Inverse[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
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

trait InverseSyntax {

  /**
   * Provides infix syntax for combining two values with an inverse
   * operation.
   */
  implicit class InverseOps[A](l: A) {

    /**
     * A symbolic alias for `inverse`.
     */
    def ~~[A1 >: A](r: => A1)(implicit inverse: Inverse[A1]): A1 =
      inverse.inverse(l, r)

    /**
     * Inverses this value with the specified value
     */
    def inverse[A1 >: A](r: => A1)(implicit inverse: Inverse[A1]): A1 =
      inverse.inverse(l, r)

    /**
     * Multiplies value 'n' times
     */
    def multiply(n: Int)(implicit inverse: Inverse[A]): A =
      inverse.multiply(n)(l)
  }
}
