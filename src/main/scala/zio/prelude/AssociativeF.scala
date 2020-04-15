package zio.prelude

object AssociativeF {
  trait Both[F[_]] {
    def both[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def associativityLaw[A: Equal, B: Equal, C: Equal](fa: F[A], fb: F[B], fc: F[C])(
      implicit invariant: Invariant[F],
      equalF: EqualF[F]
    ) = {
      val left  = both(fa, both(fb, fc))
      val right = both(both(fa, fb), fc)

      val left2 = invariant.invariantMap(Equivalence.tuple[A, B, C]).to(left)

      left2 === right
    }
  }
  object Both {
    implicit val BothOption: Both[Option] =
      new Both[Option] {
        def both[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some((a, b))
            case _                  => None
          }
      }
  }

  trait Either[F[_]] {
    def either[A, B](fa: F[A], fb: F[B]): F[scala.util.Either[A, B]]

    def associativityLaw[A: Equal, B: Equal, C: Equal](fa: F[A], fb: F[B], fc: F[C])(
      implicit invariant: Invariant[F],
      equalF: EqualF[F]
    ) = {
      val left  = either(fa, either(fb, fc))
      val right = either(either(fa, fb), fc)

      val left2 = invariant.invariantMap(Equivalence.either[A, B, C]).to(left)

      left2 === right
    }
  }
}

trait AssociativeFSyntax {
  implicit class AssociativeFOps[F[_], A](fa: F[A]) {
    def zip[B](fb: F[B])(implicit both: AssociativeF.Both[F]): F[(A, B)] =
      both.both(fa, fb)

    def orElseEither[B](fb: F[B])(implicit either: AssociativeF.Either[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }
  implicit class AssociativeFCovariantOps[F[+_], A](fa: F[A]) {
    def zipWith[B, C](fb: F[B])(f: (A, B) => C)(implicit both: AssociativeF.Both[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)

    def orElse(fa2: F[A])(implicit either: AssociativeF.Either[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }
}
