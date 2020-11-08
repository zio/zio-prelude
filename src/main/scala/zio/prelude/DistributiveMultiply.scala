package zio.prelude

import zio.prelude.coherent.DistributiveMultiplyEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait DistributiveMultiply[A] extends AddMultiplyShape[A]

object DistributiveMultiply extends Lawful[DistributiveMultiplyEqual] {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: Associative[x]] = DistributiveMultiply[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  /**
   * The left distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * a1 * (a2 + a3) === (a1 * a2) + (a1 * a3)
   * }}}
   */
  val leftDistributivityLaw: Laws[DistributiveMultiplyEqual] =
    new Laws.Law3[DistributiveMultiplyEqual]("leftDistributivityLaw") {
      def apply[A: DistributiveMultiplyEqual](a1: A, a2: A, a3: A): TestResult =
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
  val rightDistributivityLaw: Laws[DistributiveMultiplyEqual] =
    new Laws.Law3[DistributiveMultiplyEqual]("rightDistributivityLaw") {
      def apply[A: DistributiveMultiplyEqual](a1: A, a2: A, a3: A): TestResult =
        ((a1 +++ a2) *** a3) <-> ((a1 *** a3) +++ (a2 *** a3))
    }

  /**
   * The set of all laws that instances of `DistributiveMultiply` must satisfy.
   */
  val laws: Laws[DistributiveMultiplyEqual] =
    leftDistributivityLaw + rightDistributivityLaw

  /**
   * Summons an implicit `DistributiveMultiply[A]`.
   */
  def apply[A, Addition[x] <: Associative[x], Multiplication[x] <: Associative[x]](implicit
    distributiveMultiply: DistributiveMultiply.Aux[A, Addition, Multiplication]
  ): DistributiveMultiply.Aux[A, Addition, Multiplication] =
    distributiveMultiply
}
