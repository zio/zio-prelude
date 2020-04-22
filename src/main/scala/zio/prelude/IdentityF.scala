package zio.prelude

object IdentityF {
  // IdentityF.Both => IdentityBothF
  // IdentityF.Either => IdentityEitherF

  trait Both[F[_]] {
    def identity: F[Any]

    def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
  }

  trait Either[F[_]] {
    def identity: F[Nothing]

    def either[A, B](fa: => F[A], fb: => F[B]): F[scala.util.Either[A, B]]
  }
}
