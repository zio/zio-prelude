package zio.prelude

trait AssociativeFlatten[F[+_]] {
  def flatten[A](ffa: F[F[A]]): F[A]

  def associativityLaw[A](
    fffa: F[F[F[A]]]
  )(implicit covariant: Covariant[F], equalF: EqualF[F], equal: Equal[A]): Boolean = {
    val outer = flatten(flatten(fffa))
    val inner = flatten(fffa.map(flatten[A](_)))

    outer === inner
  }
}
