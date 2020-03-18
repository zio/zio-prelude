package zio.prelude

import zio.test.TestResult
import zio.test.laws.Laws

sealed trait Commutative[A] { self =>
  def combine(l: A, r: A): A

  final def commute: Commutative[A] = Commutative((l, r) => self.combine(r, l))
}

object Commutative {
  final val commutativeLaw = new Laws.Law2[Commutative with Equal]("commutativeLaw") {
    def apply[A](a1: A, a2: A)(implicit c: Commutative[A] with Equal[A]): TestResult =
      (a1 <> a2) <-> (a2 <> a1)
  }

  final val laws = commutativeLaw + Closure.laws

  def apply[A](implicit commutative: Commutative[A]): Commutative[A] = commutative

  def apply[A](f: (A, A) => A): Commutative[A] =
    new Commutative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }
}
