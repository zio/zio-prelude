package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object DistributiveProdLaws extends Lawful[DistributiveProdEqual] {

  /**
   * The left distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * a1 * (a2 + a3) === (a1 * a2) + (a1 * a3)
   * }}}
   */
  lazy val leftDistributivityLaw: Laws[DistributiveProdEqual] =
    new Laws.Law3[DistributiveProdEqual]("leftDistributivityLaw") {
      def apply[A: DistributiveProdEqual](a1: A, a2: A, a3: A): TestResult =
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
  lazy val rightDistributivityLaw: Laws[DistributiveProdEqual] =
    new Laws.Law3[DistributiveProdEqual]("rightDistributivityLaw") {
      def apply[A: DistributiveProdEqual](a1: A, a2: A, a3: A): TestResult =
        ((a1 +++ a2) *** a3) <-> ((a1 *** a3) +++ (a2 *** a3))
    }

  /**
   * The set of all laws that instances of `DistributiveProd` must satisfy.
   */
  lazy val laws: Laws[DistributiveProdEqual] =
    leftDistributivityLaw + rightDistributivityLaw

}
