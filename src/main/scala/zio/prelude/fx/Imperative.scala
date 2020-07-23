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
  implicit val ZIOImperative: Imperative[ZIO] =
    new Imperative[ZIO] {
      def succeed[A](a: => A): UIO[A] =
        ZIO.succeed(a)
      def chain[R, E, A, R1 <: R, E1 >: E, B](first: ZIO[R, E, A], f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
        first.flatMap(f)
    }
}
