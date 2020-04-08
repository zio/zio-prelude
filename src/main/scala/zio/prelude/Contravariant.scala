package zio.prelude

trait Contravariant[F[-_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]

  def identityLaw[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    contramap(identity[A](_))(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: C => B, g: B => A)(implicit equal: Equal[F[C]]): Boolean =
    contramap(f)(contramap(g)(fa)) === contramap(f andThen g)(fa)
}
object Contravariant {}
