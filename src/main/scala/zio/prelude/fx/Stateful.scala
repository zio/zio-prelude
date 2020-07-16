package zio.prelude.fx

import zio.ZIO

trait Stateful[S, F[-_, +_, +_]] extends Contextual[F] {
  def modify[A](f: S => (S, A)): F[Any, Nothing, A]

  def runState[R, E, A](s: S, v: F[R, E, A]): F[R, E, (S, A)]

  def embed[S1, R, E, A](get: S1 => S, put: (S, S1) => S1): Stateful[S, F]
}
object Stateful {
  implicit def statefulZIO[S]: Stateful[S, ({ type lambda[-R, +E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] =
    ???

  implicit def statefulZPure[S]: Stateful[S, ({ type lambda[-R, +E, +A] = ZPure[S, S, R, E, A] })#lambda] =
    ???
}
