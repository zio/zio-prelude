package zio.prelude.newtypes

import zio.prelude.Newtype

object derivation {
  def derive[A, STA <: Newtype[A], F[_]](implicit ev: STA <:< Newtype[A], typeClass: F[A]): F[STA#Type] =
    typeClass.asInstanceOf[F[STA#Type]]

  trait Auto {
    implicit def auto[A, STA <: Newtype[A], F[_]](implicit ev: STA <:< Newtype[A], typeClass: F[A]): F[STA#Type] = {
      // the type parameters and implicits are needed for 2.11 compatibility
      derive[A, STA, F](ev, typeClass)
    }
  }
  object auto extends Auto
}
