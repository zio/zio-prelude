package zio.prelude

trait Traversable[F[+_]] extends Covariant[F] {
  def fold[S, A](fa: F[A])(s: S)(f: (S, A) => S): S = ???

  def flip[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[G[A]]): G[F[A]] =
    mapEffect(fa)(identity(_))

  def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    mapEffect(fa)(f).map(_ => ())

  def map[A, B](f: A => B): F[A] => F[B]

  def mapEffect[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  // mapEffect(fa)(a => Id(a)) === Id(fa)
}
object Traversable {
  implicit val ListTraversable: Traversable[List] =
    new Traversable[List] {
      def map[A, B](f: A => B): List[A] => List[B] = (fa: List[A]) => fa.map(f)

      def mapEffect[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]] =
        list match {
          case Nil     => Nil.succeed[G]
          case a :: as => (f(a) zipWith mapEffect(as)(f))(_ :: _)
        }
    }
}
