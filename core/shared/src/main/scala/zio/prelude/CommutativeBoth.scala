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

import zio._
import zio.prelude.newtypes.{AndF, Failure, OrF}
import zio.stream.{ZSink, ZStream}

import scala.annotation.implicitNotFound

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit CommutativeBoth defined for ${F}.")
trait CommutativeBoth[F[_]] extends AssociativeBoth[F]

object CommutativeBoth {

  /**
   * Summons an implicit `CommutativeBoth[F]`.
   */
  def apply[F[_]](implicit commutativeBoth: CommutativeBoth[F]): CommutativeBoth[F] =
    commutativeBoth

  /**
   * The `CommutativeBoth` instance for `Chunk`.
   */
  implicit val ChunkCommutativeBoth: CommutativeBoth[Chunk] =
    new CommutativeBoth[Chunk] {
      def both[A, B](fa: => Chunk[A], fb: => Chunk[B]): Chunk[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `Exit`.
   */
  implicit def ExitCommutativeBoth[E]: CommutativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def both[A, B](fa: => Exit[E, A], fb: => Exit[E, B]): Exit[E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `Id`.
   */
  implicit val IdCommutativeBoth: CommutativeBoth[Id] =
    new CommutativeBoth[Id] {
      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] = Id((Id.unwrap(fa), Id.unwrap(fb)))
    }

  /**
   * The `CommutativeBoth` instance for `List`.
   */
  implicit val ListCommutativeBoth: CommutativeBoth[List] =
    new CommutativeBoth[List] {
      def both[A, B](fa: => List[A], fb: => List[B]): List[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkCommutativeBoth: CommutativeBoth[NonEmptyChunk] =
    new CommutativeBoth[NonEmptyChunk] {
      def both[A, B](fa: => NonEmptyChunk[A], fb: => NonEmptyChunk[B]): NonEmptyChunk[(A, B)] =
        (fa zipWith fb)((_, _))
    }

  /**
   * The `CommutativeBoth` instance for `Option`.
   */
  implicit val OptionCommutativeBoth: CommutativeBoth[Option] =
    new CommutativeBoth[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }

  /**
   * The `CommutativeBoth` instance for And `Schedule`.
   */
  implicit def ScheduleAndCommutativeBoth[R, E]
    : CommutativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => AndF[Schedule[R, E, A]], fb: => AndF[Schedule[R, E, B]]): AndF[Schedule[R, E, (A, B)]] =
        AndF.wrap {
          AndF.unwrap(fa) && AndF.unwrap(fb)
        }
    }

  /**
   * The `AssociativeBoth` instance for Or `Schedule`.
   */
  implicit def ScheduleOrCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => OrF[Schedule[R, E, A]], fb: => OrF[Schedule[R, E, B]]): OrF[Schedule[R, E, (A, B)]] =
        OrF.wrap {
          OrF.unwrap(fa) || OrF.unwrap(fb)
        }
    }

  /**
   * The `CommutativeBoth` instance for `Vector`.
   */
  implicit val VectorCommutativeBoth: CommutativeBoth[Vector] =
    new CommutativeBoth[Vector] {
      def both[A, B](fa: => Vector[A], fb: => Vector[B]): Vector[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `ZIO`.
   */
  implicit def ZIOCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def both[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for failed `ZIO`.
   */
  implicit def ZIOFailureCommutativeBoth[R, A]: CommutativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new CommutativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zipPar Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `CommutativeBoth` instance for `ZLayer`.
   */
  implicit def ZLayerCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def both[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `ZManaged`.
   */
  implicit def ZManagedCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def both[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureCommutativeBoth[R, A]
    : CommutativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new CommutativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zipPar Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `CommutativeBoth` instance for `ZSink`.
   */
  implicit def ZSinkCommutativeBoth[R, E, I, L]: CommutativeBoth[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] {
      def both[A, B](fa: => ZSink[R, E, I, L, A], fb: => ZSink[R, E, I, L, B]): ZSink[R, E, I, L, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `ZStream`.
   */
  implicit def ZStreamCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def both[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, (A, B)] = fa zip fb
    }

  /**
   * Combines 2 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, B](
    a0: F[A0],
    a1: F[A1]
  )(
    f: (A0, A1) => B
  ): F[B] =
    (a0 <&> a1).map(f.tupled)

  /**
   * Combines 3 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2]
  )(
    f: (A0, A1, A2) => B
  ): F[B] =
    (a0 <&> a1 <&> a2).map { case ((a0, a1), a2) =>
      f(a0, a1, a2)
    }

  /**
   * Combines 4 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3).map { case (((a0, a1), a2), a3) =>
      f(a0, a1, a2, a3)
    }

  /**
   * Combines 5 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4).map { case ((((a0, a1), a2), a3), a4) =>
      f(a0, a1, a2, a3, a4)
    }

  /**
   * Combines 6 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5).map { case (((((a0, a1), a2), a3), a4), a5) =>
      f(a0, a1, a2, a3, a4, a5)
    }

  /**
   * Combines 7 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6).map { case ((((((a0, a1), a2), a3), a4), a5), a6) =>
      f(a0, a1, a2, a3, a4, a5, a6)
    }

  /**
   * Combines 8 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7).map { case (((((((a0, a1), a2), a3), a4), a5), a6), a7) =>
      f(a0, a1, a2, a3, a4, a5, a6, a7)
    }

  /**
   * Combines 9 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8).map {
      case ((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }

  /**
   * Combines 10 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9).map {
      case (((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }

  /**
   * Combines 11 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10).map {
      case ((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }

  /**
   * Combines 12 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11).map {
      case (((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    }

  /**
   * Combines 13 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12).map {
      case ((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }

  /**
   * Combines 14 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13).map {
      case (((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    }

  /**
   * Combines 15 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14).map {
      case ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    }

  /**
   * Combines 16 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15).map {
      case (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    }

  /**
   * Combines 17 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16).map {
      case ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    }

  /**
   * Combines 18 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17).map {
      case (
            ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
            a17
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    }

  /**
   * Combines 19 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18).map {
      case (
            (
              ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
              a17
            ),
            a18
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    }

  /**
   * Combines 20 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19).map {
      case (
            (
              (
                (
                  (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                  a16
                ),
                a17
              ),
              a18
            ),
            a19
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    }

  /**
   * Combines 21 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20).map {
      case (
            (
              (
                (
                  (
                    (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                    a16
                  ),
                  a17
                ),
                a18
              ),
              a19
            ),
            a20
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    }

  /**
   * Combines 22 `F` values using the provided function `f` in parallel.
   */
  def mapN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20],
    a21: F[A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): F[B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20 <&> a21).map {
      case (
            (
              (
                (
                  (
                    (
                      (
                        ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14),
                        a15
                      ),
                      a16
                    ),
                    a17
                  ),
                  a18
                ),
                a19
              ),
              a20
            ),
            a21
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
    }

  /**
   * Combines 2 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1](
    a0: F[A0],
    a1: F[A1]
  ): F[(A0, A1)] =
    mapN(a0, a1)((_, _))

  /**
   * Combines 3 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2]
  ): F[(A0, A1, A2)] =
    mapN(a0, a1, a2)((_, _, _))

  /**
   * Combines 4 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3]
  ): F[(A0, A1, A2, A3)] =
    mapN(a0, a1, a2, a3)((_, _, _, _))

  /**
   * Combines 5 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4]
  ): F[(A0, A1, A2, A3, A4)] =
    mapN(a0, a1, a2, a3, a4)((_, _, _, _, _))

  /**
   * Combines 6 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5]
  ): F[(A0, A1, A2, A3, A4, A5)] =
    mapN(a0, a1, a2, a3, a4, a5)((_, _, _, _, _, _))

  /**
   * Combines 7 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6]
  ): F[(A0, A1, A2, A3, A4, A5, A6)] =
    mapN(a0, a1, a2, a3, a4, a5, a6)((_, _, _, _, _, _, _))

  /**
   * Combines 8 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7)((_, _, _, _, _, _, _, _))

  /**
   * Combines 9 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines 10 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)((_, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 11 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)((_, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 12 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)((_, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 13 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)((_, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 14 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 15 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[+_]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 16 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 17 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 18 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 19 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 20 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 21 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 22 `F` values into a tuple in a parallel manner.
   */
  def tupleN[F[
    +_
  ]: CommutativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20],
    a21: F[A21]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )
}

trait CommutativeBothSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeBothOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zipPar`.
     */
    def <&>[B](fb: => F[B])(implicit both: CommutativeBoth[F]): F[(A, B)] =
      zipPar(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zipPar[B](fb: => F[B])(implicit both: CommutativeBoth[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeBothCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWithPar[B, C](
      fb: => F[B]
    )(f: (A, B) => C)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeBothContraVariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWithPar[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: CommutativeBoth[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
  implicit class CommutativeBothTuple2Ops[F[+_], T1, T2](tf: => (F[T1], F[T2])) {
    def mapParN[R](f: (T1, T2) => R)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2])).tupled(tf)
  }

  implicit class CommutativeBothTuple3Ops[F[+_], T1, T2, T3](tf: => (F[T1], F[T2], F[T3])) {
    def mapParN[R](f: (T1, T2, T3) => R)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3])).tupled(tf)
  }

  implicit class CommutativeBothTuple4Ops[F[+_], T1, T2, T3, T4](tf: => (F[T1], F[T2], F[T3], F[T4])) {
    def mapParN[R](f: (T1, T2, T3, T4) => R)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4])).tupled(tf)
  }

  implicit class CommutativeBothTuple5Ops[F[+_], T1, T2, T3, T4, T5](tf: => (F[T1], F[T2], F[T3], F[T4], F[T5])) {
    def mapParN[R](f: (T1, T2, T3, T4, T5) => R)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5])).tupled(tf)
  }

  implicit class CommutativeBothTuple6Ops[F[+_], T1, T2, T3, T4, T5, T6](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6])
  ) {
    def mapParN[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6])).tupled(tf)
  }

  implicit class CommutativeBothTuple7Ops[F[+_], T1, T2, T3, T4, T5, T6, T7](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7])(f)).tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7)] =
      (CommutativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7])).tupled(tf)
  }

  implicit class CommutativeBothTuple8Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8])(f))
        .tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      (CommutativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8]))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple9Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9])(f))
        .tupled(tf)

    def tupledPar(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      (CommutativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9]))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple10Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9], _: F[T10])(
          f
        ))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      (CommutativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9], _: F[T10]))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple11Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      (CommutativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11]
        ))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple12Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      (CommutativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12]
        ))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple13Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      (CommutativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13]
        ))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple14Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14])
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      (CommutativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14]
        ))
        .tupled(tf)
  }

  implicit class CommutativeBothTuple15Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple16Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple17Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple18Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple19Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple20Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple21Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20],
      F[T21]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20],
          _: F[T21]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20],
            _: F[T21]
          )
        )
        .tupled(tf)
  }

  implicit class CommutativeBothTuple22Ops[
    F[+_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20],
      F[T21],
      F[T22]
    )
  ) {
    def mapParN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R
    )(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[R] =
      (CommutativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20],
          _: F[T21],
          _: F[T22]
        )(f))
        .tupled(tf)

    def tupledPar(implicit
      both: CommutativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
      (
        CommutativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20],
            _: F[T21],
            _: F[T22]
          )
        )
        .tupled(tf)
  }
}
