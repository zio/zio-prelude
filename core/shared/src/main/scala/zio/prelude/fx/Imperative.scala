package zio.prelude.fx

trait Imperative[F[-_, +_, +_]] {
  def succeed[A](a: => A): F[Any, Nothing, A]

  def chain[R, E, A, R1 <: R, E1 >: E, B](
    first: F[R, E, A],
    f: A => F[R1, E1, B]
  ): F[R1, E1, B]
}
