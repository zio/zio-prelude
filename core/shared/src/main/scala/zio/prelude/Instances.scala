package zio.prelude

import zio.prelude.classic.Applicative
import zio.prelude.newtypes.{ BothF, NestedF }

// not sure about putting this here
object Instances {

  object Applicative {
    def apply[F[+_]](implicit covariant: Covariant[F], identityBoth: IdentityBoth[F]): Applicative[F] = new Covariant[F]
      with IdentityBoth[F] {
      def map[A, B](f: A => B): F[A] => F[B]              =
        covariant.map(f)
      def any: F[Any]                                     =
        identityBoth.any
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] =
        identityBoth.both(fa, fb)
    }

    implicit def nestedF[F[+_], G[+_]](implicit
      C: Covariant[({ type lambda[+A] = NestedF[F, G, A] })#lambda],
      IB: IdentityBoth[({ type lambda[+A] = NestedF[F, G, A] })#lambda]
    ): Applicative[({ type lambda[+A] = NestedF[F, G, A] })#lambda] =
      new Covariant[({ type lambda[+A] = NestedF[F, G, A] })#lambda]
        with IdentityBoth[({ type lambda[+A] = NestedF[F, G, A] })#lambda] {

        def map[A, B](f: A => B): NestedF[F, G, A] => NestedF[F, G, B]                          =
          C.map(f)
        def any: NestedF[F, G, Any]                                                             =
          IB.any
        def both[A, B](fa: => NestedF[F, G, A], fb: => NestedF[F, G, B]): NestedF[F, G, (A, B)] =
          IB.both(fa, fb)
      }

    implicit def applicative[F[+_]](implicit c0: Covariant[F], i0: IdentityBoth[F]): Applicative[F] =
      Applicative[F](c0, i0)

    implicit def bothF[F[+_], G[+_]](implicit
      C: Covariant[({ type lambda[+A] = BothF[F, G, A] })#lambda],
      IB: IdentityBoth[({ type lambda[+A] = BothF[F, G, A] })#lambda]
    ): Applicative[({ type lambda[+A] = BothF[F, G, A] })#lambda] =
      new Covariant[({ type lambda[+A] = BothF[F, G, A] })#lambda]
        with IdentityBoth[({ type lambda[+A] = BothF[F, G, A] })#lambda] {

        def map[A, B](f: A => B): BothF[F, G, A] => BothF[F, G, B]                          =
          C.map(f)
        def any: BothF[F, G, Any]                                                           =
          IB.any
        def both[A, B](fga: => BothF[F, G, A], fgb: => BothF[F, G, B]): BothF[F, G, (A, B)] =
          IB.both(fga, fgb)
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
