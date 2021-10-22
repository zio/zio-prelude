package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object AbsorptionLaws extends Lawful[AbsorptionEqual] {

  /**
   * The join absorption law states that for the join operator `vvv`, the meet operator `^^^`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * a1 vvv (a1 ^^^ a2) === a1
   * }}}
   */
  lazy val joinAbsorptionLaw: Laws[AbsorptionEqual] =
    new Laws.Law2[AbsorptionEqual]("joinAbsorptionLaw") {
      def apply[A](a1: A, a2: A)(implicit A: AbsorptionEqual[A]): TestResult =
        (a1 vvv (a1 ^^^ a2)) <-> a1
    }

  /**
   * The meet absorption law states that for the join operator `vvv`, the meet operator `^^^`,
   * and for any value `a`, the following must hold:
   *
   * {{{
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
