package zio.prelude.fx

import zio.prelude.Contravariant

trait Contextual[F[-_, +_, +_]] {
  def fallible[R]: Fallible[({ type lambda[+E, +A] = F[R, E, A] })#lambda]

  def contravariant[E, A]: Contravariant[({ type lambda[-R] = F[R, E, A] })#lambda]

  def environment[R]: F[R, Nothing, R]

  def provide[R, E, A](v: F[R, E, A], r: R): F[Any, E, A]
}
