package zio.prelude

trait IdentityFlatten[F[+_]] extends AssociativeFlatten[F] { self =>
  def any: F[Any]

  def rightIdentity[A](fa: F[A])(implicit covariant: Covariant[F], equalF: EqualF[F], equalA: Equal[A]): Boolean = {
    val ffa = fa.map((a: A) => any.map(_ => a))

    flatten(ffa) === fa
  }

  def leftIdentity[A](fa: F[A])(implicit covariant: Covariant[F], equalF: EqualF[F], equalA: Equal[A]): Boolean = {
    val ffa = any.map(_ => fa)

    flatten(ffa) === fa
  }
}
object IdentityFlatten {
  implicit val IdentityFlattenOption: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any] = Some(())

      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }
}
