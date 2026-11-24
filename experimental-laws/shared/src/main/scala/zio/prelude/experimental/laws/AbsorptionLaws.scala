package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `Absorption` instances must satisfy, checked with `vvv`, `^^^` and
 * `Equal`. They belong to the experimental module, so they can change between
 * releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(AbsorptionLaws)(Gen.setOf(Gen.int))
 * // succeeds with a passing TestResult
 * }}}
 */
object AbsorptionLaws extends Lawful[AbsorptionEqual] {

  /**
   * The join absorption law states that for the join operator `vvv`, the meet
   * operator `^^^`, and for all values `a1` and `a2`, the following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * a1 vvv (a1 ^^^ a2) === a1
   * }}}
   */
  lazy val joinAbsorptionLaw: Laws[AbsorptionEqual] =
    new Laws.Law2[AbsorptionEqual]("joinAbsorptionLaw") {
      def apply[A](a1: A, a2: A)(implicit A: AbsorptionEqual[A]): TestResult =
        (a1 vvv (a1 ^^^ a2)) <-> a1
    }

  /**
   * The meet absorption law states that for the join operator `vvv`, the meet
   * operator `^^^`, and for all values `a1` and `a2`, the following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * a1 ^^^ (a1 vvv a2) === a1
   * }}}
   */
  lazy val meetAbsorptionLaw: Laws[AbsorptionEqual] =
    new Laws.Law2[AbsorptionEqual]("meetAbsorptionLaw") {
      def apply[A](a1: A, a2: A)(implicit A: AbsorptionEqual[A]): TestResult =
        (a1 ^^^ (a1 vvv a2)) <-> a1
    }

  /**
   * The set of all laws that instances of `Absorption` must satisfy.
   */
  lazy val laws: Laws[AbsorptionEqual] =
    joinAbsorptionLaw + meetAbsorptionLaw
}
