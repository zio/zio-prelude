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

/**
 * The `Identity` type class describes an associative binary operator for a
 * type `A` that also has an identity element. Combining any value with the
 * identity element on either the left or the right must return the original
 * value unchanged. For example, zero is an identity element for integer
 * addition and the empty string is an identity element for string
 * concatenation.
 *
 * Operators with an identity element are useful because the identity element
 * provides a sensible default value when combining values of a type and no
 * values exist.
 */
trait Identity[A] extends Associative[A] {

  /**
   * The identity element.
   */
  def identity: A

  override def multiplyOption(n: Int)(a: A): Option[A] =
    if (n < 0) None
    else if (n == 0) Some(identity)
    else super.multiplyOption(n)(a)
}

object Identity {

  /**
   * Summons an implicit `Identity[A]`.
   */
  def apply[A](implicit Identity: Identity[A]): Identity[A] = Identity

  /**
   * Constructs an `Identity` instance from an `IdentityEither` instance and a
   * `Covariant` instance.
   */
  def fromIdentityEitherCovariant[F[+_]: IdentityEither: Covariant, A]: Identity[F[A]] =
    new Identity[F[A]] {
      def identity: F[A]                        =
        IdentityEither[F].none
      def combine(l: => F[A], r: => F[A]): F[A] =
        l orElse r
    }

  /**
   * Constructs an `Identity` instance from an associative binary operator and
   * an identity element.
   */
  def make[A](identity0: A, op: (A, A) => A): Identity[A] =
    new Identity[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
    }

  /**
   * Constructs an `Identity` instance from an associative instance and
   * an identity element.
   */
  def makeFrom[A](identity0: A, associative: Associative[A]): Identity[A] =
    make(identity0, (l, r) => associative.combine(l, r))

  /**
   * Derives an `Identity[F[A]]` given a `Derive[F, Identity]` and an
   * `Identity[A]`.
   */
  implicit def DeriveIdentity[F[_], A](implicit derive: Derive[F, Identity], identity: Identity[A]): Identity[F[A]] =
    derive.derive(identity)

  /**
   * Derives an `Identity[Either[E, A]]` given an `Identity[A]`.
   */
  implicit def EitherIdentity[E, A: Identity]: Identity[Either[E, A]] =
    makeFrom(
      Right(Identity[A].identity),
      Associative.EitherAssociative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple2Identity[A: Identity, B: Identity]: Identity[(A, B)] =
    makeFrom(
      (Identity[A].identity, Identity[B].identity),
      Associative.Tuple2Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple3Identity[A: Identity, B: Identity, C: Identity]: Identity[(A, B, C)] =
    makeFrom(
      (Identity[A].identity, Identity[B].identity, Identity[C].identity),
      Associative.Tuple3Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple4Identity[A: Identity, B: Identity, C: Identity, D: Identity]: Identity[(A, B, C, D)] =
    makeFrom(
      (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity),
      Associative.Tuple4Associative
    )

  implicit def Tuple5Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity]
    : Identity[(A, B, C, D, E)] =
    makeFrom(
      (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity, Identity[E].identity),
      Associative.Tuple5Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple6Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity]
    : Identity[(A, B, C, D, E, F)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity
      ),
      Associative.Tuple6Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple7Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity, G: Identity]
    : Identity[(A, B, C, D, E, F, G)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity
      ),
      Associative.Tuple7Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple8Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity
  ]: Identity[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity
      ),
      Associative.Tuple8Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple9Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity
      ),
      Associative.Tuple9Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple10Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity
      ),
      Associative.Tuple10Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple11Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity
      ),
      Associative.Tuple11Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple12Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity
      ),
      Associative.Tuple12Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple13Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity
      ),
      Associative.Tuple13Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple14Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity
      ),
      Associative.Tuple14Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple15Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity
      ),
      Associative.Tuple15Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple16Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity
      ),
      Associative.Tuple16Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple17Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity
      ),
      Associative.Tuple17Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple18Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity,
        Identity[R].identity
      ),
      Associative.Tuple18Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple19Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity,
        Identity[R].identity,
        Identity[S].identity
      ),
      Associative.Tuple19Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple20Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity,
        Identity[R].identity,
        Identity[S].identity,
        Identity[T].identity
      ),
      Associative.Tuple20Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple21Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity,
        Identity[R].identity,
        Identity[S].identity,
        Identity[T].identity,
        Identity[U].identity
      ),
      Associative.Tuple21Associative
    )

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple22Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity,
    V: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(
      (
        Identity[A].identity,
        Identity[B].identity,
        Identity[C].identity,
        Identity[D].identity,
        Identity[E].identity,
        Identity[F].identity,
        Identity[G].identity,
        Identity[H].identity,
        Identity[I].identity,
        Identity[J].identity,
        Identity[K].identity,
        Identity[L].identity,
        Identity[M].identity,
        Identity[N].identity,
        Identity[O].identity,
        Identity[P].identity,
        Identity[Q].identity,
        Identity[R].identity,
        Identity[S].identity,
        Identity[T].identity,
        Identity[U].identity,
        Identity[V].identity
      ),
      Associative.Tuple22Associative
    )
}

trait IdentitySyntax {

  /**
   * Provides infix syntax for combining two values with an associative
   * operation.
   */
  implicit class IdentityOps[A](l: A) {

    /**
     * Returns the identity element associated with values of this type.
     */
    def identity(implicit id: Identity[A]): A =
      id.identity
  }

}
