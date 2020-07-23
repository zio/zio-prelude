package zio.prelude.fx

import zio._

trait Imperative[F[-_, +_, +_]] {
  def succeed[A](a: => A): F[Any, Nothing, A]

  def chain[R, E, A, R1 <: R, E1 >: E, B](
    first: F[R, E, A],
    f: A => F[R1, E1, B]
  ): F[R1, E1, B]
}

object Imperative {

  def apply[F[-_, +_, +_]](imperative: Imperative[F]): Imperative[F] =
    imperative

  /**
   * The `Imperative` instance for `ZIO`.
   */
  implicit def ZIOImperative[S]: Imperative[({ type lambda[-R, +E, +A] = StatefulZIO[S, R, E, A] })#lambda] =
    new Imperative[({ type lambda[-R, +E, +A] = StatefulZIO[S, R, E, A] })#lambda] {
      def succeed[A](a: => A): StatefulZIO[S, Any, Nothing, A] =
        ZIO.succeed(a)
      def chain[R, E, A, R1 <: R, E1 >: E, B](
        first: StatefulZIO[S, R, E, A],
        f: A => StatefulZIO[S, R1, E1, B]
      ): StatefulZIO[S, R1, E1, B] =
        first.flatMap(f)
    }
}
