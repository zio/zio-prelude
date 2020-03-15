package zio.prelude

trait LeftIdentityLaws[A] extends AssociativeLaws[A] {
  def leftIdentity: A

  def combine(l: A, r: A): A

  final def leftIdentityLaw(a: A)(implicit equal: Equal[A]): Boolean =
    combine(leftIdentity, a) === a
}
sealed trait LeftIdentity[A] extends LeftIdentityLaws[A]
object LeftIdentity {
  def apply[A](implicit leftIdentity: LeftIdentity[A]): LeftIdentity[A] = leftIdentity
}
