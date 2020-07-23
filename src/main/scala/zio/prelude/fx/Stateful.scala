package zio.prelude.fx

import zio.{ Ref, ZIO }

trait Stateful[F[_, -_, +_, +_]] {

  def contextual[S]: Contextual[({ type lambda[-R, +E, +A] = F[S, R, E, A] })#lambda]

  def embed[S1, S2, R, E, A](v: F[S1, R, E, A])(get: S2 => S1, put: (S1, S2) => S2): F[S2, R, E, A]

  def modify[S, A](f: S => (S, A)): F[S, Any, Nothing, A]

  def runState[S, R, E, A](s: S, v: F[S, R, E, A]): F[S, R, E, (S, A)]
}

object Stateful {

  /**
   * The `Stateful` instance for `ZIO`.
   */
  implicit val ZIOStateful: Stateful[StatefulZIO] =
    new Stateful[StatefulZIO] {
      def contextual[S]: Contextual[({ type lambda[-R, +E, +A] = StatefulZIO[S, R, E, A] })#lambda] =
        Contextual.ZIOContextual
      def embed[S1, S2, R, E, A](
        v: StatefulZIO[S1, R, E, A]
      )(get: S2 => S1, put: (S1, S2) => S2): StatefulZIO[S2, R, E, A] =
        v.provideSome {
          case (r, ref) =>
            (r, ref.foldAll(identity, identity, identity, s1 => s2 => Right(put(s1, s2)), s2 => Right(get(s2))))
        }
      def modify[S, A](f: S => (S, A)): StatefulZIO[S, Any, Nothing, A] =
        ZIO.accessM(_._2.modify(f(_).swap))
      def runState[S, R, E, A](s: S, v: StatefulZIO[S, R, E, A]): StatefulZIO[S, R, E, (S, A)] =
        for {
          _ <- ZIO.accessM[(Any, Ref[S])](_._2.set(s))
          a <- v
          s <- ZIO.accessM[(Any, Ref[S])](_._2.get)
        } yield (s, a)
    }
}
