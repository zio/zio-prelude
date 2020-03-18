package zio.prelude

import zio.test.laws.Lawful

sealed trait FullIdentity[A] {
  def identity: A

  final def leftIdentity: A = identity

  final def rightIdentity: A = identity

  def combine(l: A, r: A): A
}

object FullIdentity
    extends Lawful[FullIdentity with LeftIdentity with RightIdentity with Associative with Closure with Equal] {

  final val laws = LeftIdentity.leftIdentityLaw + RightIdentity.laws

  def apply[A](implicit fullIdentity: FullIdentity[A]): FullIdentity[A] = fullIdentity

  def apply[A](identity0: A, op: (A, A) => A): FullIdentity[A] =
    new FullIdentity[A] {
      def identity: A = identity0

      def combine(l: A, r: A): A = op(l, r)
    }

  implicit val stringFullIdentity: FullIdentity[String] = FullIdentity[String]("", (l: String, r: String) => l + r)
  implicit val intFullIdentity: FullIdentity[Int]       = FullIdentity[Int](0, (l: Int, r: Int) => l + r)
  implicit val longFullIdentity: FullIdentity[Long]     = FullIdentity[Long](0L, (l: Long, r: Long) => l + r)
}
