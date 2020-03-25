package zio.prelude

import zio.test.laws.Lawful

trait Identity[A] extends LeftIdentity[A] with RightIdentity[A] {
  def identity: A

  override final def leftIdentity: A = identity

  override final def rightIdentity: A = identity
}

object Identity extends Lawful[Identity with Equal] {

  final val laws = LeftIdentity.laws + RightIdentity.laws

  def apply[A](implicit Identity: Identity[A]): Identity[A] = Identity

  def apply[A](identity0: A, op: (A, A) => A): Identity[A] =
    new Identity[A] {
      def identity: A = identity0

      def combine(l: A, r: A): A = op(l, r)
    }

  implicit val stringIdentity: Identity[String] = Identity[String]("", (l: String, r: String) => l + r)
  implicit val intIdentity: Identity[Int]       = Identity[Int](0, (l: Int, r: Int) => l + r)
  implicit val longIdentity: Identity[Long]     = Identity[Long](0L, (l: Long, r: Long) => l + r)
}
