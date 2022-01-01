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

import zio.{Cause, Chunk, Exit, NonEmptyChunk}

import scala.util.Try

/**
 * `Derive[F, Typeclass]` represents a universally quantified function from
 * `Typeclass[A]` to `Typeclass[F[A]]` for some `F[_]`. You can think of
 * `Derive` as a "recipe" for building a `Typeclass[F[A]]` instance given a
 * `Typeclass[A]`.
 *
 * For example, if we know how to compare values of type `A` for equality then
 * we can compare lists with elements of type `A` for equality by checking
 * that the length of the lists is the same and each pair of corresponding
 * elements are equal. And we can do this for any type `A` as long as it has
 * an `Equal` instance.
 *
 * This is used by the library to derive typeclass instances for higher kinded
 * types given typeclass instances for the type they are parameterized on.
 */
trait Derive[F[_], Typeclass[_]] {
  def derive[A: Typeclass]: Typeclass[F[A]]
}

object Derive {

  /**
   * Summon an implicit `Derive[F, Typeclass]`
   */
  def apply[F[_], Typeclass[_]](implicit derive: Derive[F, Typeclass]): Derive[F, Typeclass] =
    derive

  /**
   * The `DeriveEqual` instance for `Chunk`.
   */
  implicit val ChunkDeriveEqual: Derive[Chunk, Equal] =
    new Derive[Chunk, Equal] {
      def derive[A: Equal]: Equal[Chunk[A]] =
        Equal.ChunkEqual
    }

  /**
   * The `DeriveEqual` instance for `List`.
   */
  implicit val ListDeriveEqual: Derive[List, Equal] =
    new Derive[List, Equal] {
      def derive[A: Equal]: Equal[List[A]] =
        Equal.ListEqual
    }

  /**
   * The `DeriveEqual` instance for `Either`.
   */
  implicit def EitherDeriveEqual[E: Equal]: DeriveEqual[({ type lambda[+x] = Either[E, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = Either[E, x] })#lambda] {
      def derive[A: Equal]: Equal[Either[E, A]] =
        Equal.EitherEqual
    }

  /**
   * The `DeriveEqual` instance for `Map`.
   */
  implicit def MapDeriveEqual[A]: DeriveEqual[({ type lambda[+x] = Map[A, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = Map[A, x] })#lambda] {
      def derive[B: Equal]: Equal[Map[A, B]] =
        Equal.MapPartialOrd
    }

  /**
   * The `DeriveEqual` instance for  `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkDeriveEqual: DeriveEqual[NonEmptyChunk] = new DeriveEqual[NonEmptyChunk] {
    def derive[A: Equal]: Equal[NonEmptyChunk[A]] =
      Equal.NonEmptyChunkEqual
  }

  /**
   * The `DeriveEqual` instance for `Option`.
   */
  implicit val OptionDeriveEqual: DeriveEqual[Option] =
    new DeriveEqual[Option] {
      def derive[A: Equal]: Equal[Option[A]] =
        Equal.OptionEqual
    }

  /**
   * The `DeriveEqual` instance for `ParSeq`.
   */
  implicit def ParSeqDeriveEqual[Z <: Unit]: DeriveEqual[({ type lambda[+x] = ParSeq[Z, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = ParSeq[Z, x] })#lambda] {
      def derive[A: Equal]: Equal[ParSeq[Z, A]] =
        ParSeq.parSeqHash
    }

  /**
   * The `DeriveEqual` instance for `Set`.
   */
  implicit val SetDeriveEqual: DeriveEqual[Set] =
    new DeriveEqual[Set] {
      def derive[A: Equal]: Equal[Set[A]] =
        Equal.SetHashPartialOrd
    }

  /**
   * The `DeriveEqual` instance for `Try`.
   */
  implicit val TryDeriveEqual: DeriveEqual[Try] =
    new DeriveEqual[Try] {
      def derive[A: Equal]: Equal[Try[A]] =
        Equal.TryEqual
    }

  /**
   * The `DeriveEqual` instance for `Tuple2`.
   */
  implicit def Tuple2DeriveEqual[A: Equal]: DeriveEqual[({ type lambda[x] = (A, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, x) })#lambda] {
      def derive[B: Equal]: Equal[(A, B)] =
        Equal.Tuple2Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple3`.
   */
  implicit def Tuple3DeriveEqual[A: Equal, B: Equal]: DeriveEqual[({ type lambda[x] = (A, B, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, x) })#lambda] {
      def derive[C: Equal]: Equal[(A, B, C)] =
        Equal.Tuple3Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple4`.
   */
  implicit def Tuple4DeriveEqual[A: Equal, B: Equal, C: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, x) })#lambda] {
      def derive[D: Equal]: Equal[(A, B, C, D)] =
        Equal.Tuple4Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple5`.
   */
  implicit def Tuple5DeriveEqual[A: Equal, B: Equal, C: Equal, D: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, D, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, x) })#lambda] {
      def derive[E: Equal]: Equal[(A, B, C, D, E)] =
        Equal.Tuple5Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple6`.
   */
  implicit def Tuple6DeriveEqual[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, D, E, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, x) })#lambda] {
      def derive[F: Equal]: Equal[(A, B, C, D, E, F)] =
        Equal.Tuple6Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple7`.
   */
  implicit def Tuple7DeriveEqual[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, x) })#lambda] {
      def derive[G: Equal]: Equal[(A, B, C, D, E, F, G)] =
        Equal.Tuple7Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple8`.
   */
  implicit def Tuple8DeriveEqual[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, x) })#lambda] {
      def derive[H: Equal]: Equal[(A, B, C, D, E, F, G, H)] =
        Equal.Tuple8Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple9`.
   */
  implicit def Tuple9DeriveEqual[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal, H: Equal]
    : DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, x) })#lambda] {
      def derive[I: Equal]: Equal[(A, B, C, D, E, F, G, H, I)] =
        Equal.Tuple9Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple10`.
   */
  implicit def Tuple10DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, x) })#lambda] {
      def derive[J: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J)] =
        Equal.Tuple10Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple11`.
   */
  implicit def Tuple11DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, x) })#lambda] {
      def derive[K: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K)] =
        Equal.Tuple11Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple12`.
   */
  implicit def Tuple12DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, x) })#lambda] {
      def derive[L: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L)] =
        Equal.Tuple12Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple13`.
   */
  implicit def Tuple13DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, x) })#lambda] {
      def derive[M: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
        Equal.Tuple13Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple14`.
   */
  implicit def Tuple14DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, x) })#lambda] {
      def derive[N: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
        Equal.Tuple14Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple15`.
   */
  implicit def Tuple15DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, x) })#lambda] {
      def derive[O: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
        Equal.Tuple15Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple16`.
   */
  implicit def Tuple16DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, x) })#lambda] {
      def derive[P: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
        Equal.Tuple16Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple17`.
   */
  implicit def Tuple17DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, x) })#lambda] {
      def derive[Q: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
        Equal.Tuple17Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple18`.
   */
  implicit def Tuple18DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, x) })#lambda] {
      def derive[R: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
        Equal.Tuple18Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple19`.
   */
  implicit def Tuple19DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, x) })#lambda] {
      def derive[S: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
        Equal.Tuple19Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple20`.
   */
  implicit def Tuple20DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, x) })#lambda] {
      def derive[T: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
        Equal.Tuple20Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple21`.
   */
  implicit def Tuple21DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, x) })#lambda] {
      def derive[U: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
        Equal.Tuple21Equal
    }

  /**
   * The `DeriveEqual` instance for `Tuple22`.
   */
  implicit def Tuple22DeriveEqual[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal,
    U: Equal
  ]: DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, x) })#lambda] =
    new DeriveEqual[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, x) })#lambda] {
      def derive[V: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
        Equal.Tuple22Equal
    }

  /**
   * The `DeriveEqual` instance for `Vector`.
   */
  implicit val VectorDeriveEqual: DeriveEqual[Vector] =
    new DeriveEqual[Vector] {
      def derive[A: Equal]: Equal[Vector[A]] =
        Equal.make(_.corresponds(_)(_ === _))
    }

  /**
   * The `DeriveEqual` instance for `Cause`.
   */
  implicit val CauseDeriveEqual: DeriveEqual[Cause] =
    new DeriveEqual[Cause] {
      def derive[A: Equal]: Equal[Cause[A]] =
        Equal.CauseHash
    }

  /**
   * The `DeriveEqual` instance for `Exit`.
   */
  implicit def ExitDeriveEqual[E: Equal]: DeriveEqual[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new DeriveEqual[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def derive[A: Equal]: Equal[Exit[E, A]] =
        Equal.ExitEqual
    }
}
