package zio.prelude

trait RightIdentityLaws[A] extends AssociativeLaws[A] {
  def rightIdentity: A

  def combine(l: A, r: A): A

  final def RightIdentityLaw(a: A)(implicit equal: Equal[A]): Boolean =
    combine(a, rightIdentity) === a
}
sealed trait RightIdentity[A] extends RightIdentityLaws[A]
object RightIdentity {
  def apply[A](implicit rightIdentity: RightIdentity[A]): RightIdentity[A] = rightIdentity
}
