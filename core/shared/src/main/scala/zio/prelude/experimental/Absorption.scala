package zio.prelude
package experimental

import zio.prelude.experimental.coherent.AbsorptionEqual
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait Absorption[A] extends JoinMeetShape[A]

object Absorption extends Lawful[AbsorptionEqual] {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Absorption[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

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

  /**
   * Summons an implicit `Absorption[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    absorption: Absorption.Aux[A, Join, Meet]
  ): Absorption.Aux[A, Join, Meet] =
    absorption
}
