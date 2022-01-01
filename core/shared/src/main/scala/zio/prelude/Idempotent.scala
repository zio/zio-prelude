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
 * The `Idempotent` type class describes a binary operator for a type `A` that
 * is both associative and produces the same value when combining two identical values.
 * This means that `a <> a` is equal to `a` for all values `a`.
 * Example of idempotent operations is union of sets, but not addition of integers.
 *
 * Idempotent operators are useful because combining  the values with an idempotent
 * operation results in the same value regardless of the number of values
 * are combined, allowing us to optimize out unnecessary combinations of the same values.
 */
trait Idempotent[A] extends Associative[A] { self =>

  protected def combineNormal(l: => A, r: => A): A = combine(l, r)

  /**
   * If the values are equal, it doesn't attempt to combine them and promptly returns the first one.
   * If not equal, the values are combined normally.
   */
  final def combineIdempotent(l: => A, r: => A)(implicit A: Equal[A]): A =
    if (l === r) l else combineNormal(l, r)

  /** Creates a new instance which internally uses `combineIdempotent` for combining values. */
  def idempotent(implicit A: Equal[A]): Idempotent[A] = new Idempotent[A] {

    override protected def combineNormal(l: => A, r: => A): A = self.combineNormal(l, r)

    override def combine(l: => A, r: => A): A = combineIdempotent(l, r)
  }

}

object Idempotent {

  /**
   * Summons an implicit `Idempotent[A]`.
   */
  def apply[A](implicit idempotent: Idempotent[A]): Idempotent[A] = idempotent

  /**
   * Constructs an `Idempotent` instance from a associative binary operator.
   */
  def make[A](f: (A, A) => A): Idempotent[A] =
    new Idempotent[A] {
      override def combine(l: => A, r: => A): A = f(l, r)
    }

  /**
   * Constructs an `Idempotent` instance from a associative instance.
   */
  def makeFrom[A](associative: Associative[A]): Idempotent[A] =
    make((l, r) => associative.combine(l, r))

  /**
   * Derives a `Idempotent[F[A]]` given a `Derive[F, Idempotent]` and a
   * `Idempotent[A]`.
   */
  implicit def DeriveIdempotent[F[_], A](implicit
    derive: Derive[F, Idempotent],
    idempotent: Idempotent[A]
  ): Idempotent[F[A]] =
    derive.derive(idempotent)

  /**
   * Derives a `Idempotent[Map[K, V]]` given a `Idempotent[V]`.
   */
  implicit def MapIdempotent[K, V: Idempotent]: Idempotent[Map[K, V]] =
    makeFrom(Associative.MapIdentity)

  /**
   * Derives a `Idempotent[Option[A]]` given a `Idempotent[A]`
   */
  implicit def OptionIdempotent[A: Idempotent]: Idempotent[Option[A]] =
    makeFrom(Associative.OptionIdentity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple2Idempotent[A: Idempotent, B: Idempotent]: Idempotent[(A, B)] =
    makeFrom(Associative.Tuple2Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple3Idempotent[A: Idempotent, B: Idempotent, C: Idempotent]: Idempotent[(A, B, C)] =
    makeFrom(Associative.Tuple3Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple4Idempotent[A: Idempotent, B: Idempotent, C: Idempotent, D: Idempotent]: Idempotent[(A, B, C, D)] =
    makeFrom(Associative.Tuple4Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple5Idempotent[A: Idempotent, B: Idempotent, C: Idempotent, D: Idempotent, E: Idempotent]
    : Idempotent[(A, B, C, D, E)] =
    makeFrom(Associative.Tuple5Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple6Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent
  ]: Idempotent[(A, B, C, D, E, F)] =
    makeFrom(Associative.Tuple6Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple7Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G)] =
    makeFrom(Associative.Tuple7Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple8Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H)] =
    makeFrom(Associative.Tuple8Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple9Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(Associative.Tuple9Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple10Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(Associative.Tuple10Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple11Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(Associative.Tuple11Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple12Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(Associative.Tuple12Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple13Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(Associative.Tuple13Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple14Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(Associative.Tuple14Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple15Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(Associative.Tuple15Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple16Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(Associative.Tuple16Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple17Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(Associative.Tuple17Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple18Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(Associative.Tuple18Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple19Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(Associative.Tuple19Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple20Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(Associative.Tuple20Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple21Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent,
    U: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(Associative.Tuple21Associative)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple22Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent,
    U: Idempotent,
    V: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(Associative.Tuple22Associative)
}
