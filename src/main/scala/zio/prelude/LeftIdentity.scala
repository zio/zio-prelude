package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

sealed trait LeftIdentity[A] {
  def leftIdentity: A

  def combine(l: A, r: A): A
}

object LeftIdentity extends Lawful[LeftIdentity with Equal with Closure with Associative] {
  final val leftIdentityLaw = new Laws.Law1[LeftIdentity with Equal with Closure with Associative]("leftIdentityLaw") {
    def apply[A](a: A)(implicit L: LeftIdentity[A] with Equal[A] with Closure[A] with Associative[A]): TestResult =
      (L.leftIdentity <> a) <-> a
  }

  final val laws = leftIdentityLaw + Associative.laws

  def apply[A](implicit leftIdentity: LeftIdentity[A]): LeftIdentity[A] = leftIdentity
}
