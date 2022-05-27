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
import zio.prelude.newtypes.Failure
import zio.stream.ZStream

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit AssociativeEither defined for ${F}.")
trait AssociativeEither[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object AssociativeEither {

  /**
   * Summons an implicit `AssociativeEither[F]`.
   */
  def apply[F[_]](implicit associativeEither: AssociativeEither[F]): AssociativeEither[F] =
    associativeEither

  /**
   * The `IdentityEither` instance for `Chunk`.
   */
  implicit val ChunkIdentityEither: IdentityEither[Chunk] =
    new IdentityEither[Chunk] {
      def either[A, B](fa: => Chunk[A], fb: => Chunk[B]): Chunk[Either[A, B]] =
        fa.map(Left(_)) ++ fb.map(Right(_))
      val none: Chunk[Nothing]                                                =
        Chunk.empty
    }

  /**
   * The `AssociativeEither` instance for `Either`.
   */
  implicit def EitherAssociativeEither[L]: AssociativeEither[({ type lambda[+r] = Either[L, r] })#lambda] =
    new AssociativeEither[({ type lambda[+r] = Either[L, r] })#lambda] {
      def either[A, B](fa: => Either[L, A], fb: => Either[L, B]): Either[L, Either[A, B]] =
        fa.map(Left(_)).left.flatMap(_ => fb.map(Right(_)))
    }

  /**
   * The `AssociativeEither` instance for a failed `Either`
   */
  implicit def EitherFailedAssociativeEither[R]
    : AssociativeEither[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new AssociativeEither[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      def either[A, B](fa: => Failure[Either[A, R]], fb: => Failure[Either[B, R]]): Failure[Either[Either[A, B], R]] =
        Failure.wrap {
          Failure
            .unwrap(fa)
            .left
            .map(Left(_))
            .flatMap(_ => Failure.unwrap(fb).left.map(Right(_)))
        }
    }

  /**
   * The `AssociativeEither` instance for `Exit`.
   */
  implicit def ExitAssociativeEither[E]: AssociativeEither[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def either[A, B](fa: => Exit[E, A], fb: => Exit[E, B]): Exit[E, Either[A, B]] =
        fa.map(Left(_)) match {
          case Exit.Failure(_) => fb.map(Right(_))
          case res             => res
        }
    }

  /**
   * The `AssociativeEither` instance for failed `Exit`.
   */
  implicit def ExitFailureAssociativeEither[A]: AssociativeEither[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] {
      def either[EA, EB](fa: => Failure[Exit[EA, A]], fb: => Failure[Exit[EB, A]]): Failure[Exit[Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `Fiber`.
   */
  implicit def FiberAssociativeEither[E]: AssociativeEither[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def either[A, B](fa: => Fiber[E, A], fb: => Fiber[E, B]): Fiber[E, Either[A, B]] =
        fa.orElseEither(fb)
    }

  /**
   * The `AssociativeEither` instance for `Future`.
   */
  implicit def FutureAssociativeEither(implicit ec: ExecutionContext): AssociativeEither[Future] =
    new AssociativeEither[Future] {
      def either[A, B](fa: => Future[A], fb: => Future[B]): Future[Either[A, B]] =
        fa.map(Left(_)).recoverWith { case _: Throwable =>
          fb.map(Right(_))
        }
    }

  /**
   * The `IdentityEither` instance for `List`.
   */
  implicit val ListIdentityEither: IdentityEither[List] =
    new IdentityEither[List] {
      def either[A, B](fa: => List[A], fb: => List[B]): List[Either[A, B]] =
        fa.map(Left(_)) ::: fb.map(Right(_))
      val none: List[Nothing]                                              =
        List.empty
    }

  /**
   * The `IdentityEither` (and `AssociativeEither`) instance for `Option`.
   */
  implicit val OptionIdentityEither: IdentityEither[Option] =
    new IdentityEither[Option] {
      def either[A, B](fa: => Option[A], fb: => Option[B]): Option[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))

      val none: Option[Nothing] =
        None
    }

  /**
   * The `AssociativeEither` instance for `Schedule`.
   */
  implicit def ScheduleAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = Schedule[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = Schedule[R, E, a] })#lambda] {
      def either[A, B](fa: => Schedule[R, E, A], fb: => Schedule[R, E, B]): Schedule[R, E, Either[A, B]] =
        fa.andThenEither(fb)
    }

  /**
   * The `CommutativeEither` and `IdentityEither` instance for `Set`.
   */
  implicit val SetCommutativeEitherIdentityEither: CommutativeEither[Set] with IdentityEither[Set] =
    new CommutativeEither[Set] with IdentityEither[Set] {
      def either[A, B](fa: => Set[A], fb: => Set[B]): Set[Either[A, B]] =
        fa.map(Left(_)) ++ fb.map(Right(_))
      val none: Set[Nothing]                                            =
        Set.empty
    }

  /**
   * The `AssociativeEither` instance for `Try`.
   */
  implicit val TryAssociativeEither: AssociativeEither[Try] =
    new AssociativeEither[Try] {
      def either[A, B](fa: => Try[A], fb: => Try[B]): Try[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `IdentityEither` instance for `Vector`.
   */
  implicit val VectorIdentityEither: IdentityEither[Vector] =
    new IdentityEither[Vector] {
      def either[A, B](fa: => Vector[A], fb: => Vector[B]): Vector[Either[A, B]] =
        fa.map(Left(_)) ++ fb.map(Right(_))
      val none: Vector[Nothing]                                                  =
        Vector.empty
    }

  /**
   * The `AssociativeEither` instance for `ZIO`.
   */
  implicit def ZIOAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def either[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, Either[A, B]] =
        fa.orElseEither(fb)
    }

  /**
   * The `AssociativeEither` instance for failed `ZIO`.
   */
  implicit def ZIOFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `ZLayer`.
   */
  implicit def ZLayerAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def either[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for `ZManaged`.
   */
  implicit def ZManagedAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def either[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, Either[A, B]] =
        fa.orElseEither(fb)
    }

  /**
   * The `AssociativeEither` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `ZStream`.
   */
  implicit def ZStreamAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def either[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for failed `ZStream`.
   */
  implicit def ZStreamFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZStream[R, EA, A]],
        fb: => Failure[ZStream[R, EB, A]]
      ): Failure[ZStream[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }
}

trait AssociativeEitherSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeEitherOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEither`.
     */
    def <+>[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      orElseEither(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEither[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines an `F[A]` value with itself using `orElse` until it eventually
     * returns the result of the left value without having to evaluate the
     * right value.
     */
    def eventually(implicit either: AssociativeEither[F], covariant: Covariant[F]): F[A] =
      fa orElse eventually

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElse(fa2: => F[A])(implicit either: AssociativeEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWith[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: AssociativeEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
