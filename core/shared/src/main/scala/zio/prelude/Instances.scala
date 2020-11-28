package zio.prelude

import zio.prelude.classic.Applicative

// not sure about putting this here
object Instances {

  object Applicative {
    def apply[F[+_]: Applicative]: Applicative[F] = new Covariant[F] with IdentityBoth[F] { self =>
      def map[A, B](f: A => B): F[A] => F[B] = Covariant[F].map(f)

      def any: F[Any] = IdentityBoth[F].any

      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = IdentityBoth[F].both(fa, fb)
    }

    final def compose[F[+_]: Applicative, G[+_]: Applicative](
      af: Applicative[F],
      ag: Applicative[G]
    ): Applicative[({ type lambda[+A] = F[G[A]] })#lambda] =
      new Covariant[({ type lambda[+A] = F[G[A]] })#lambda] with IdentityBoth[({ type lambda[+A] = F[G[A]] })#lambda] {
        def map[A, B](f: A => B): F[G[A]] => F[G[B]] = af.map(ag.map(f))

        def any: F[G[Any]] = af.map[Any, G[Any]](_ => ag.any)(af.any)

        def both[A, B](fa: => F[G[A]], fb: => F[G[B]]): F[G[(A, B)]] =
          af.map[(G[A], G[B]), G[(A, B)]] { case (ga, gb) => ag.both(ga, gb) }(af.both(fa, fb))
      }
  }

  trait ApplicativeDeriveEqual[F[+_]] extends Covariant[F] with IdentityBoth[F] with DeriveEqual[F]

  object ApplicativeDeriveEqual {
    implicit def derive[F[+_]](implicit
      applicative0: Applicative[F],
      deriveEqual0: DeriveEqual[F]
    ): ApplicativeDeriveEqual[F] =
      new ApplicativeDeriveEqual[F] {
        def any: F[Any]                                     =
          applicative0.any
        def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] =
          applicative0.both(fa, fb)
        def derive[A: Equal]: Equal[F[A]]                   =
          deriveEqual0.derive
        def map[A, B](f: A => B): F[A] => F[B]              =
          applicative0.map(f)
      }
  }
}
