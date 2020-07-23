package zio.prelude.fx

import zio.{ Ref, ZIO }
import zio.prelude.Contravariant

trait Contextual[F[-_, +_, +_]] {
  def fallible[R]: Fallible[({ type lambda[+E, +A] = F[R, E, A] })#lambda]

  def contravariant[E, A]: Contravariant[({ type lambda[-R] = F[R, E, A] })#lambda]

  def environment[R]: F[R, Nothing, R]

  def provide[R, E, A](v: F[R, E, A], r: R): F[Any, E, A]
}

object Contextual {

  implicit def ZIOContextual[S]: Contextual[({ type lambda[-R, +E, +A] = StatefulZIO[S, R, E, A] })#lambda] =
    new Contextual[({ type lambda[-R, +E, +A]          = StatefulZIO[S, R, E, A] })#lambda] {
      def fallible[R]: Fallible[({ type lambda[+E, +A] = StatefulZIO[S, R, E, A] })#lambda] =
        Fallible.ZIOFallible[(R, Ref[S])]
      def contravariant[E, A]: Contravariant[({ type lambda[-R] = StatefulZIO[S, R, E, A] })#lambda] =
        new Contravariant[({ type lambda[-R] = StatefulZIO[S, R, E, A] })#lambda] {
          def contramap[R, R1](f: R1 => R): ZIO[(R, Ref[S]), E, A] => ZIO[(R1, Ref[S]), E, A] =
            _.provideSome { case (r, ref) => (f(r), ref) }
        }
      def environment[R]: StatefulZIO[S, R, Nothing, R] =
        ZIO.access(_._1)
      def provide[R, E, A](v: StatefulZIO[S, R, E, A], r: R): StatefulZIO[S, Any, E, A] =
        v.provideSome { case (_, ref) => (r, ref) }
    }
}
