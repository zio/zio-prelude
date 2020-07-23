package zio.prelude.fx

import zio._
import zio.prelude.Contravariant

trait Stateful[S, F[-_, +_, +_]] extends Contextual[F] {
  def modify[A](f: S => (S, A)): F[Any, Nothing, A]

  def runState[R, E, A](s: S, v: F[R, E, A]): F[R, E, (S, A)]

  def embed[S1, R, E, A](get: S1 => S, put: (S, S1) => S1): Stateful[S1, F]
}

object Stateful {

  /**
   * The `Stateful` instance for `ZIO`.
   */
  implicit def ZIOStateful[S: Tag]: Stateful[S, ({ type lambda[-R, +E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] =
    new Stateful[S, ({ type lambda[-R, +E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] {
      def modify[A](f: S => (S, A)): ZIO[RefState[S], Nothing, A] =
        ZIO.accessM(_.get.modify(f))
      def runState[R, E, A](s: S, v: ZIO[RefState[S] with R, E, A]): ZIO[RefState[S] with R, E, (S, A)] =
        for {
          _ <- ZIO.accessM[RefState[S]](_.get.set(s))
          a <- v
          s <- ZIO.accessM[RefState[S]](_.get.get)
        } yield (s, a)
      def embed[S1, R, E, A](
        get: S1 => S,
        put: (S, S1) => S1
      ): Stateful[S1, ({ type lambda[-R, +E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] =
        ???
      def environment[R]: ZIO[RefState[S] with R, Nothing, R] =
        ZIO.environment[R]
      def provide[R, E, A](v: ZIO[RefState[S] with R, E, A], r: R): ZIO[RefState[S], E, A] =
        ???
      def fallible[R]: Fallible[({ type lambda[+E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] =
        new Fallible[({ type lambda[+E, +A] = ZIO[RefState[S] with R, E, A] })#lambda] {
          def absolve[E, A](v: ZIO[RefState[S] with R, E, Either[E, A]]): ZIO[RefState[S] with R, E, A] =
            v.absolve
          def recover[E, A](v: ZIO[RefState[S] with R, E, A]): ZIO[RefState[S] with R, Nothing, Either[E, A]] =
            v.either
        }
      def contravariant[E, A]: Contravariant[({ type lambda[-R] = ZIO[RefState[S] with R, E, A] })#lambda] =
        new Contravariant[({ type lambda[-R] = ZIO[RefState[S] with R, E, A] })#lambda] {
          def contramap[R, R0](f: R0 => R): ZIO[RefState[S] with R, E, A] => ZIO[RefState[S] with R0, E, A] =
            ???
        }
    }
}
