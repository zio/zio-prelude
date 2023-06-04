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

import zio._
import zio.stm.ZSTM
import zio.stream.ZStream

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.{Success, Try}

/**
 * `AssociativeFlatten` describes a type that can be "flattened" in an
 * associative way. For example, if we have a list of lists of lists, we can
 * flatten it by either flattening the two inner lists and then flattening the
 * resulting lists, or flattening the two outer lists and then flattening that
 * resulting list. Because the operation is associative, the resulting list is
 * the same either way.
 */
@implicitNotFound("No implicit AssociativeFlatten defined for ${F}.")
trait AssociativeFlatten[F[+_]] {

  /**
   * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
   */
  def flatten[A](ffa: F[F[A]]): F[A]
}

object AssociativeFlatten {

  /**
   * Summons an implicit `AssociativeFlatten[F]`.
   */
  def apply[F[+_]](implicit associativeFlatten: AssociativeFlatten[F]): AssociativeFlatten[F] =
    associativeFlatten

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Cause`.
   */
  implicit val CauseIdentityFlatten: IdentityFlatten[Cause] =
    new IdentityFlatten[Cause] {
      override def any: Cause[Any] = Cause.fail(())

      override def flatten[A](ffa: Cause[Cause[A]]): Cause[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Chunk`.
   */
  implicit val ChunkIdentityFlatten: IdentityFlatten[Chunk] =
    new IdentityFlatten[Chunk] {
      def any: Chunk[Any] = Chunk.unit

      def flatten[A](ffa: Chunk[Chunk[A]]): Chunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Const`.
   */
  implicit def ConstAssociativeFlatten[A]: AssociativeFlatten[({ type ConstA[+B] = Const[A, B] })#ConstA] =
    new AssociativeFlatten[({ type ConstA[+B] = Const[A, B] })#ConstA] {
      def flatten[B](ffb: Const[A, Const[A, B]]): Const[A, B] =
        Const.wrap(Const.unwrap(ffb))
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Either`.
   */
  implicit def EitherIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] {
      def any: Either[E, Any] = Right(())

      def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] =
        ffa match {
          case Left(e)         => Left(e)
          case Right(Left(e))  => Left(e)
          case Right(Right(a)) => Right(a)
        }
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Exit`.
   */
  implicit def ExitIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def any: Exit[E, Any] = Exit.unit

      def flatten[A](ffa: Exit[E, Exit[E, A]]): Exit[E, A] = ffa.flattenExit
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Future`.
   */
  implicit val FutureIdentityFlatten: IdentityFlatten[Future] =
    new IdentityFlatten[Future] {
      def any: Future[Any] = Future.successful(())

      def flatten[A](ffa: Future[Future[A]]): Future[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Id`.
   */
  implicit val IdIdentityFlatten: IdentityFlatten[Id] =
    new IdentityFlatten[Id] {
      def any: Id[Any] = Id(())

      def flatten[A](ffa: Id[Id[A]]): Id[A] = Id.unwrap(ffa)
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `List`.
   */
  implicit val ListIdentityFlatten: IdentityFlatten[List] =
    new IdentityFlatten[List] {
      def any: List[Any] = List(())

      def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Map`
   */
  implicit def MapAssociativeFlatten[K]: AssociativeFlatten[({ type lambda[+v] = Map[K, v] })#lambda] =
    new AssociativeFlatten[({ type lambda[+v] = Map[K, v] })#lambda] {
      def flatten[V](ffa: Map[K, Map[K, V]]): Map[K, V] =
        ffa.foldLeft[Map[K, V]](Map.empty) { case (l, (_, r)) => r.foldLeft(l)(_ + _) }
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkIdentityFlatten: IdentityFlatten[NonEmptyChunk] =
    new IdentityFlatten[NonEmptyChunk] {
      def any: NonEmptyChunk[Any] = NonEmptyChunk.single(())

      def flatten[A](ffa: NonEmptyChunk[NonEmptyChunk[A]]): NonEmptyChunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Option`.
   */
  implicit val OptionIdentityFlatten: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any] = Some(())

      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Try`.
   */
  implicit val TryIdentityFlatten: IdentityFlatten[Try] =
    new IdentityFlatten[Try] {
      def any: Try[Any] = Success(())

      def flatten[A](ffa: Try[Try[A]]): Try[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Vector`.
   */
  implicit val VectorIdentityFlatten: IdentityFlatten[Vector] =
    new IdentityFlatten[Vector] {
      def any: Vector[Any] = Vector(())

      def flatten[A](ffa: Vector[Vector[A]]): Vector[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZIO`.
   */
  implicit def ZIOIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def any: ZIO[R, E, Any] = ZIO.unit

      def flatten[A](ffa: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZSTM`.
   */
  implicit def ZSTMIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] {
      def any: ZSTM[R, E, Any] = ZSTM.unit

      def flatten[A](ffa: ZSTM[R, E, ZSTM[R, E, A]]): ZSTM[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZStream`.
   */
  implicit def ZStreamIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def any: ZStream[R, E, Any] = ZStream.unit

      def flatten[A](ffa: ZStream[R, E, ZStream[R, E, A]]): ZStream[R, E, A] = ffa.flatten
    }

}

trait AssociativeFlattenSyntax {

  /**
   * Provides infix syntax for flattening types.
   */
  implicit class AssociativeFlattenOps[F[+_], A](ffa: F[F[A]]) {

    /**
     * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
     */
    def flatten(implicit flatten: AssociativeFlatten[F]): F[A] =
      flatten.flatten(ffa)
  }

  /**
   * Provides infix syntax for flattening covariant types.
   */
  implicit class AssociativeFlattenCovariantOps[F[+_], A](fa: F[A]) {

    /**
     * Maps a function `A => F[B]` over an `F[A]` value and then flattens the
     * resulting `F[F[B]]`.
     */
    def flatMap[B](f: A => F[B])(implicit flatten: AssociativeFlatten[F], covariant: Covariant[F]): F[B] =
      flatten.flatten(covariant.map(f)(fa))
  }

  /**
   * Provides infix syntax for filtering covariant types.
   */
  implicit class AssociativeFlattenCovariantIdentityBothIdentityEitherOps[F[+_], A](fa: F[A]) {

    /**
     * Collects values of `A` for which the partial function `pf` is defined.
     */
    def collect[B](pf: PartialFunction[A, B])(implicit
      flatten: AssociativeFlatten[F],
      covariant: Covariant[F],
      identityBoth: IdentityBoth[F],
      identityEither: IdentityEither[F]
    ): F[B] =
      fa.flatMap(a => pf.lift(a).fold[F[B]](identityEither.none)(_.succeed))

    /**
     * Collects values of `A` for which the partial function `pf` is defined.
     */
    def collectM[B](pf: PartialFunction[A, F[B]])(implicit
      flatten: AssociativeFlatten[F],
      covariant: Covariant[F],
      identityEither: IdentityEither[F]
    ): F[B] =
      fa.flatMap(a => pf.lift(a).getOrElse(identityEither.none))

    /**
     * Filters an `F[A]` value with a predicate `f`.
     */
    def filter[B](f: A => Boolean)(implicit
      flatten: AssociativeFlatten[F],
      covariant: Covariant[F],
      identityBoth: IdentityBoth[F],
      identityEither: IdentityEither[F]
    ): F[A] =
      fa.flatMap(a => if (f(a)) a.succeed else identityEither.none)

    /**
     * Filters an `F[A]` value with a predicate `f`.
     */
    def filterM[B](f: A => F[Boolean])(implicit
      flatten: AssociativeFlatten[F],
      covariant: Covariant[F],
      identityBoth: IdentityBoth[F],
      identityEither: IdentityEither[F]
    ): F[A] =
      fa.flatMap(a => f(a).flatMap(b => if (b) a.succeed else identityEither.none))

    /**
     * Provides support for filtering `F[A]` values in for comphrensions.
     */
    def withFilter(f: A => Boolean)(implicit
      flatten: AssociativeFlatten[F],
      covariant: Covariant[F],
      identityBoth: IdentityBoth[F],
      identityEither: IdentityEither[F]
    ): F[A] =
      filter(f)
  }
}
