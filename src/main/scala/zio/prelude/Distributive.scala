package zio.prelude

import zio.prelude.coherent.DistributiveEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Distributive[A, +Addition[x] <: Associative[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiply[A, Addition, Multiplication]

object Distributive extends Lawful[DistributiveEqual] {

  /**
   * The left distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * a1 * (a2 + a3) === (a1 * a2) + (a1 * a3)
   * }}}
   */
  val leftDistributivityLaw: Laws[DistributiveEqual] =
    new Laws.Law3[DistributiveEqual]("leftDistributivityLaw") {
      def apply[A: DistributiveEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 *** (a2 +++ a3)) <-> ((a1 *** a2) +++ (a1 *** a3))
    }

  /**
   * The right distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 + a2) * a3 === (a1 * a3) + (a2 * a3)
   * }}}
   */
  val rightDistributivityLaw: Laws[DistributiveEqual] =
    new Laws.Law3[DistributiveEqual]("rightDistributivityLaw") {
      def apply[A: DistributiveEqual](a1: A, a2: A, a3: A): TestResult =
        ((a1 +++ a2) *** a3) <-> ((a1 *** a3) +++ (a2 *** a3))
    }

  /**
   * The set of all laws that instances of `Distributive` must satisfy.
   */
  val laws: Laws[DistributiveEqual] =
    leftDistributivityLaw + rightDistributivityLaw

  /**
   * Summons an implicit `Distributive[A]`.
   */
  def apply[A, Addition[x] <: Associative[x], Multiplication[x] <: Associative[x]](implicit
    distributive: Distributive[A, Addition, Multiplication]
  ): Distributive[A, Addition, Multiplication] =
    distributive
}
