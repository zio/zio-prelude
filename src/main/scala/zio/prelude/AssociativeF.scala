package zio.prelude

object AssociativeF {
  trait Tuple[F[_]] {
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def associativityLaw[A: Equal, B: Equal, C: Equal](fa: F[A], fb: F[B], fc: F[C])(
      implicit invariant: Invariant[F],
      equalF: EqualF[F]
    ) = {
      val left  = zip(fa, zip(fb, fc))
      val right = zip(zip(fa, fb), fc)

      val left2 = invariant.invariantMap(Equivalence.tuple[A, B, C]).to(left)

      left2 === right
    }
  }
  object Tuple {
    implicit val TupleOption: Tuple[Option] =
      new Tuple[Option] {
        def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some((a, b))
            case _                  => None
          }
      }
  }

  trait Either[F[_]] {
    def orElseEither[A, B](fa: F[A], fb: F[B]): F[scala.util.Either[A, B]]
  }
}
