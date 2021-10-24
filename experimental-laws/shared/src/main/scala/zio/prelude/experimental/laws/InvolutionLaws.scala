package zio.prelude.experimental.laws

import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object InvolutionLaws extends Lawful[InvolutionEqual] {

  /**
   * The join involution law states that for the complement operator `!`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !(!a) === a
   * }}}
   */
  lazy val involutionLaw: Laws[InvolutionEqual] =
    new Laws.Law1[InvolutionEqual]("involutionLaw") {
      def apply[A](a: A)(implicit A: InvolutionEqual[A]): TestResult =
        A.complement(A.complement(a)) <-> a
    }

  /**
   * The set of all laws that instances of `Involution` must satisfy.
   */
  lazy val laws: Laws[InvolutionEqual] =
    AbsorptionLaws.laws + involutionLaw

}
