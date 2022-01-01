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

import zio.ZIO
import zio.stream.{ZSink, ZStream}

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit CommutativeEither defined for ${F}.")
trait CommutativeEither[F[_]] extends AssociativeEither[F]

object CommutativeEither {

  /**
   * The `CommutativeEither` instance for `Future`.
   */
  implicit def FutureCommutativeEither(implicit ec: ExecutionContext): CommutativeEither[Future] =
    new CommutativeEither[Future] {
      def either[A, B](fa: => Future[A], fb: => Future[B]): Future[Either[A, B]] =
        Promise[Either[A, B]]().completeWith(fa.map(Left(_))).completeWith(fb.map(Right(_))).future
    }

  /**
   * The `CommutativeEither` instance for `ZIO`.
   */
  implicit def ZIOCommutativeEither[R, E]: CommutativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def either[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, Either[A, B]] =
        fa.raceEither(fb)
    }

  /**
   * The `CommutativeEither` instance for `ZSink`.
   */
  implicit def ZSinkCommutativeEither[R, E, I, L]
    : CommutativeEither[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] {
      def either[A, B](fa: => ZSink[R, E, I, L, A], fb: => ZSink[R, E, I, L, B]): ZSink[R, E, I, L, Either[A, B]] =
        fa.raceBoth(fb)
    }

  /**
   * The `CommutativeEither` instance for `ZStream`.
   */
  implicit def ZStreamCommutativeEither[R, E]: CommutativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def either[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, Either[A, B]] =
        fa mergeEither fb
    }

  /**
   * Summons an implicit `CommutativeEither[F]`.
   */
  def apply[F[_]](implicit commutativeEither: CommutativeEither[F]): CommutativeEither[F] =
    commutativeEither
}

trait CommutativeEitherSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeEitherOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEitherPar`.
     */
    def <|>[B](fb: => F[B])(implicit either: CommutativeEither[F]): F[Either[A, B]] =
      orElseEitherPar(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEitherPar[B](fb: => F[B])(implicit either: CommutativeEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElsePar(fa2: => F[A])(implicit either: CommutativeEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWithPar[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: CommutativeEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
