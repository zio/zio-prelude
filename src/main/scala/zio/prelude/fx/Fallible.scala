package zio.prelude.fx

import zio._

trait Fallible[F[+_, +_]] {
  def absolve[E, A](v: F[E, Either[E, A]]): F[E, A]

  def recover[E, A](v: F[E, A]): F[Nothing, Either[E, A]]
}

object Fallible {

  /**
   * Summons an implicit `Fallible[F]`.
   */
  def apply[F[+_, +_]](fallible: Fallible[F]): Fallible[F] =
    fallible

  /**
   * The `Fallible` instance for `ZIO`.
   */
  implicit def ZIOFallible[R]: Fallible[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] =
    new Fallible[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] {
      def absolve[E, A](v: ZIO[R, E, Either[E, A]]): ZIO[R, E, A] =
        v.absolve
      def recover[E, A](v: ZIO[R, E, A]): ZIO[R, Nothing, Either[E, A]] =
        v.either
    }
}
