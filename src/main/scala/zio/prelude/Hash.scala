package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` has a Hash(`A`)
 */
trait Hash[-A] extends Equal[A] { self =>
  def hash(a: A): Int
}

object Hash extends Lawful[Hash] {
  final val consistencyLaw = new Laws.Law2[Hash]("consistencyLaw") {
    def apply[A](a1: A, a2: A)(implicit caps: Hash[A]): TestResult =
      (a1 <-> a2) <==> (Hash[A].hash(a1) <-> Hash[A].hash(a2))
  }

  final val laws = consistencyLaw

  def apply[A](implicit hash: Hash[A]): Hash[A] = hash

  def fromFunctions[A](f: A => Int, eq0: (A, A) => Boolean): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int                     = f(a)
      override def equal(l: A, r: A): Boolean = eq0(l, r)
    }

}

trait HashSyntax {

  implicit class HashSyntax[A](a: A) {
    def hash(implicit hash: Hash[A]): Int = hash.hash(a)

    def ##(implicit hash: Hash[A]): Int = hash.hash(a)
  }

}
