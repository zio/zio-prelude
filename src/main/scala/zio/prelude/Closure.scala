package zio.prelude

import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait ClosureLaws[A] {
  def combine(l: A, r: A): A
}

sealed trait Closure[A] extends ClosureLaws[A]

object Closure extends ClosureImplicits0 with Lawful[Closure] {
  final val closureLaw = new Laws.Law2[Closure]("closureLaw") {
    def apply[A: Closure] (a1: A, a2: A): TestResult =
      (try {
        a1 <> a2
        true
      } catch { case _: Throwable => false }) <-> true
  }

  final val laws = closureLaw

  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def apply[A](f: (A, A) => A): Closure[A] =
    new Closure[A] {
      def combine(l: A, r: A): A = f(l, r)
    }
}
private[prelude] trait ClosureImplicits1 {
  implicit def CommutativeDerivesClosure[A](implicit commutative: Commutative[A]): Closure[A] =
    Closure(commutative.combine(_, _))
}
private[prelude] trait ClosureImplicits0 extends ClosureImplicits1 {
  implicit def AssociativeDerivesClosure[A](implicit associative: Associative[A]): Closure[A] =
    Closure(associative.combine(_, _))
}
trait ClosureSyntax {
  implicit class ClosureSyntax[A](l: A) {
    def combine(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)

    def <>(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)
  }
}
