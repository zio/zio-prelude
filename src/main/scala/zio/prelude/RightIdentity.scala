package zio.prelude

import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

sealed trait RightIdentity[A] {
  def rightIdentity: A

  def combine(l: A, r: A): A
}

object RightIdentity extends Lawful[RightIdentity with Equal with Closure with Associative] {
  final val rightIdentityLaw = new Laws.Law1[RightIdentity with Equal with Closure with Associative]("rightIdentityLaw") {
    def apply[A](a: A)(implicit R: RightIdentity[A] with Equal[A] with Closure[A] with Associative[A]): TestResult =
      (a <> R.rightIdentity) <-> a
  }

  final val laws = rightIdentityLaw + Associative.laws

  def apply[A](implicit rightIdentity: RightIdentity[A]): RightIdentity[A] = rightIdentity
}
