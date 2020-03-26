package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Associative[A]` type class describes an associative binary operator
 * for a type `A`. For example, addition for integers, and string
 * concatenation for strings.
 */
trait Associative[A] extends Closure[A]

object Associative extends Lawful[Associative with Equal] {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  final val associativityLaw = new Laws.Law3[Associative with Equal]("associativityLaw") {
    def apply[A](a1: A, a2: A, a3: A)(implicit A: Associative[A] with Equal[A]): TestResult =
      (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
  }

  final val laws = associativityLaw + Closure.laws

  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  def fromFunction[A](f: (A, A) => A): Associative[A] =
    new Associative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

  implicit def lastAssociative[A]: Associative[Last[A]] =
    fromFunction((_: Last[A], r: Last[A]) => r)

  implicit def firstAssociative[A]: Associative[First[A]] =
    fromFunction((l: First[A], _: First[A]) => l)

  implicit def minAssociative[A: Ord]: Associative[Min[A]] =
    fromFunction((l: Min[A], r: Min[A]) => if (l < r) l else r)

  implicit def maxAssociative[A: Ord]: Associative[Max[A]] =
    fromFunction((l: Max[A], r: Max[A]) => if (l > r) l else r)
}
