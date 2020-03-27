package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Closure[A] {
  def combine(l: A, r: A): A
}

object Closure extends Lawful[Closure] {

  final val closureLaw = new Laws.Law2[Closure]("closureLaw") {
    def apply[A: Closure](a1: A, a2: A): TestResult =
      (try {
        (a1 <> a2) != null
      } catch { case _: Throwable => false }) <-> true
  }

  final val laws = closureLaw

  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def make[A](f: (A, A) => A): Closure[A] =
    new Closure[A] {
      def combine(l: A, r: A): A = f(l, r)
    }
}

trait ClosureSyntax {

  implicit class ClosureSyntax[A](l: A) {
    def combine(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)

    def <>(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)
  }

}
