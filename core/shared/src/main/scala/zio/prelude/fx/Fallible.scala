package zio.prelude.fx

trait Fallible[F[+_, +_]] {
  def absolve[E, A](v: F[E, Either[E, A]]): F[E, A]

  def recover[E, A](v: F[E, A]): F[Nothing, Either[E, A]]
}
