package zio.prelude

import zio.test.laws.Lawful

sealed trait Identity[A] {
  def identity: A

  final def leftIdentity: A = identity

  final def rightIdentity: A = identity

  def combine(l: A, r: A): A
}

object Identity extends Lawful[Identity with LeftIdentity with RightIdentity with Associative with Closure with Equal] {

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
