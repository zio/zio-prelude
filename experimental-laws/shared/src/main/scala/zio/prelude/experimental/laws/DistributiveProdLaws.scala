package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `DistributiveProd` instances must satisfy, checked with `+++`,
 * `***` and `Equal`. They belong to the experimental module, so they can change
 * between releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(DistributiveProdLaws)(Gen.long)
 * // succeeds with a passing TestResult
 * }}}
 */
object DistributiveProdLaws extends Lawful[DistributiveProdEqual] {

  /**
   * The left distributivity law states that for the operators `+++` (`sum`)
   * and `***` (`prod`), for all values `a1`, `a2` and `a3`, the following must
   * hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * a1 *** (a2 +++ a3) === (a1 *** a2) +++ (a1 *** a3)
   * }}}
   */
  lazy val leftDistributivityLaw: Laws[DistributiveProdEqual] =
    new Laws.Law3[DistributiveProdEqual]("leftDistributivityLaw") {
      def apply[A: DistributiveProdEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 *** (a2 +++ a3)) <-> ((a1 *** a2) +++ (a1 *** a3))
    }

  /**
   * The right distributivity law states that for the operators `+++` (`sum`)
   * and `***` (`prod`), for all values `a1`, `a2` and `a3`, the following must
   * hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * (a1 +++ a2) *** a3 === (a1 *** a3) +++ (a2 *** a3)
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
