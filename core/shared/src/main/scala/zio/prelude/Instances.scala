package zio.prelude

import zio.prelude.AssociativeBoth.OptionIdentityBoth
import zio.prelude.Invariant.OptionTraversable
import zio.prelude.classic.Applicative
import zio.prelude.newtypes.{ Nested, Product }

// not sure about putting this here
object Instances {

  object Applicative {
    def apply[F[+_]: Covariant: IdentityBoth]: Applicative[F] = new Covariant[F] with IdentityBoth[F] { self =>
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

    implicit def nested0[F[+_], G[+_]](implicit
      F: Applicative[F],
      G: Applicative[G]
    ): Applicative[({ type lambda[+A] = Nested[F, G, A] })#lambda] =
      new Covariant[({ type lambda[+A] = Nested[F, G, A] })#lambda]
        with IdentityBoth[({ type lambda[+A] = Nested[F, G, A] })#lambda] {
        private lazy val FG = F.compose(G)

        override def map[A, B](f: A => B): Nested[F, G, A] => Nested[F, G, B]                         = (fga: Nested[F, G, A]) =>
          Nested(FG.map(f)(Nested.unwrap[F[G[A]]](fga)))
        override def any: Nested[F, G, Any]                                                           =
          Nested(G.any.succeed[F])
        override def both[A, B](fa: => Nested[F, G, A], fb: => Nested[F, G, B]): Nested[F, G, (A, B)] =
          Nested(Nested.unwrap[F[G[A]]](fa).zipWith(Nested.unwrap[F[G[B]]](fb))(_ zip _))
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

    implicit val applicativeOption: Applicative[Option] = Applicative[Option](OptionTraversable, OptionIdentityBoth)

    implicit def product0[F[+_], G[+_]](implicit
      F: Applicative[F],
      G: Applicative[G]
    ): Applicative[({ type lambda[+A] = Product[F, G, A] })#lambda] =
      new Covariant[({ type lambda[+A] = Product[F, G, A] })#lambda]
        with IdentityBoth[({ type lambda[+A] = Product[F, G, A] })#lambda] {
        private lazy val FG = Applicative.product(F, G)

        override def map[A, B](f: A => B): Product[F, G, A] => Product[F, G, B] = (fga: Product[F, G, A]) =>
          Product(FG.map(f)(Product.unwrap(fga)))

        override def any: Product[F, G, Any] = Product((F.any, G.any))

        override def both[A, B](fga: => Product[F, G, A], fgb: => Product[F, G, B]): Product[F, G, (A, B)] =
          Product(FG.both(Product.unwrap(fga), Product.unwrap(fgb)))
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
