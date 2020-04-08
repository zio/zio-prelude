package zio.prelude

trait Covariant[F[+_]] {
  def map[A, B](f: A => B): F[A] => F[B]

  def identityLaw[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    map(identity[A](_))(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A => B, g: B => C)(implicit equal: Equal[F[C]]): Boolean =
    map(g)(map(f)(fa)) === map(f andThen g)(fa)
}
object Covariant {
  implicit val CovariantOption: Covariant[Option] =
    new Covariant[Option] {
      def map[A, B](f: A => B): Option[A] => Option[B] = {
        case Some(a) => Some(f(a))
        case None    => None
      }
    }

}
