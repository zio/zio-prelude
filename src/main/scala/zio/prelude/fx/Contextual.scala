package zio.prelude.fx

import zio.ZIO
import zio.prelude.Contravariant

trait Contextual[F[-_, +_, +_]] {
  def fallible[R]: Fallible[({ type lambda[+E, +A] = F[R, E, A] })#lambda]

  def contravariant[E, A]: Contravariant[({ type lambda[-R] = F[R, E, A] })#lambda]

  def environment[R]: F[R, Nothing, R]

  def provide[R, E, A](v: F[R, E, A], r: R): F[Any, E, A]
}

object Contextual {

  implicit val ZIOContextual: Contextual[ZIO] =
    new Contextual[ZIO] {
      def environment[R]: ZIO[R, Nothing, R] =
        ZIO.environment[R]
      def provide[R, E, A](v: ZIO[R, E, A], r: R): ZIO[Any, E, A] =
        v.provide(r)
      def fallible[R]: Fallible[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] =
        new Fallible[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] {
          def absolve[E, A](v: ZIO[R, E, Either[E, A]]): ZIO[R, E, A] =
            v.absolve
          def recover[E, A](v: ZIO[R, E, A]): ZIO[R, Nothing, Either[E, A]] =
            v.either
        }
      def contravariant[E, A]: Contravariant[({ type lambda[-R] = ZIO[R, E, A] })#lambda] =
        new Contravariant[({ type lambda[-R] = ZIO[R, E, A] })#lambda] {
          def contramap[R, R0](f: R0 => R): ZIO[R, E, A] => ZIO[R0, E, A] =
            _.provideSome(f)
        }
    }
}
