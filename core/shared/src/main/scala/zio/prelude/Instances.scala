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
      F: Applicative[F],
      G: Applicative[G]
    ): Applicative[({ type lambda[+A] = F[G[A]] })#lambda] =
      new Covariant[({ type lambda[+A] = F[G[A]] })#lambda] with IdentityBoth[({ type lambda[+A] = F[G[A]] })#lambda] {
        def map[A, B](f: A => B): F[G[A]] => F[G[B]] = F.map(G.map(f))

        def any: F[G[Any]] = F.map[Any, G[Any]](_ => G.any)(F.any)

        def both[A, B](fa: => F[G[A]], fb: => F[G[B]]): F[G[(A, B)]] =
          F.map[(G[A], G[B]), G[(A, B)]] { case (ga, gb) => G.both(ga, gb) }(F.both(fa, fb))
      }

    final def product[F[+_]: Applicative, G[+_]: Applicative](
      F: Applicative[F],
      G: Applicative[G]
    ): Applicative[({ type lambda[+A] = (F[A], G[A]) })#lambda] =
      new Covariant[({ type lambda[+A] = (F[A], G[A]) })#lambda]
        with IdentityBoth[({ type lambda[+A] = (F[A], G[A]) })#lambda] {
        def map[A, B](f: A => B): ((F[A], G[A])) => (F[B], G[B]) =
          (faga: (F[A], G[A])) => (faga._1.map(f), faga._2.map(f))

        def any: (F[Any], G[Any]) = (F.any, G.any)

        def both[A, B](faga: => (F[A], G[A]), fbgb: => (F[B], G[B])): (F[(A, B)], G[(A, B)]) =
          (F.both(faga._1, fbgb._1), G.both(faga._2, fbgb._2))
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
