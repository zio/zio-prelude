package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait RightIdentity[A] extends Associative[A] {
  def rightIdentity: A
}

object RightIdentity extends Lawful[RightIdentity with Equal] {

  final val rightIdentityLaw = new Laws.Law1[RightIdentity with Equal]("rightIdentityLaw") {
    def apply[A](a: A)(implicit R: RightIdentity[A] with Equal[A]): TestResult =
      (a <> R.rightIdentity) <-> a
  }

  final val laws = rightIdentityLaw + Associative.laws

  def apply[A](implicit rightIdentity: RightIdentity[A]): RightIdentity[A] = rightIdentity
}
