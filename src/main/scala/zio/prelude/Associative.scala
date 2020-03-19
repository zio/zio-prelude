package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

sealed trait Associative[A] {
  def combine(l: A, r: A): A
}

object Associative extends Lawful[Associative with Closure with Equal] {
  final val associativityLaw = new Laws.Law3[Associative with Closure with Equal]("associativityLaw") {
    def apply[A] (a1: A, a2: A, a3: A)(implicit A: Associative[A] with Closure[A] with Equal[A]): TestResult =
      (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
  }

  final val laws = associativityLaw + Closure.laws

  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  def apply[A](f: (A, A) => A): Associative[A] =
    new Associative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

}
