package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait LeftIdentity[A] extends Associative[A] {
  def leftIdentity: A
}

object LeftIdentity extends Lawful[LeftIdentity with Equal] {

  final val leftIdentityLaw = new Laws.Law1[LeftIdentity with Equal]("leftIdentityLaw") {
    def apply[A](a: A)(implicit L: LeftIdentity[A] with Equal[A]): TestResult =
      (L.leftIdentity <> a) <-> a
  }

  final val laws = leftIdentityLaw + Associative.laws

  def apply[A](implicit leftIdentity: LeftIdentity[A]): LeftIdentity[A] = leftIdentity
}
