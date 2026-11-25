package zio.prelude.experimental.laws

import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `Involution` instances must satisfy, checked with `complement` and
 * `Equal`. They include the `ExcludedMiddleLaws` and the
 * `NoncontradictionLaws`. They belong to the experimental module, so they can
 * change between releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(InvolutionLaws)(Gen.boolean)
 * // succeeds with a passing TestResult
 * }}}
 */
object InvolutionLaws extends Lawful[InvolutionEqual] {

  /**
   * The involution law states that for the complement operator `!`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * !(!a) === a
   * }}}
   */
  lazy val involutionLaw: Laws[InvolutionEqual] =
    new Laws.Law1[InvolutionEqual]("involutionLaw") {
      def apply[A](a: A)(implicit A: InvolutionEqual[A]): TestResult =
        A.complement(A.complement(a)) <-> a
    }

  /**
   * The set of all laws that instances of `Involution` must satisfy: the
   * `ExcludedMiddleLaws`, the `NoncontradictionLaws` and the involution law.
   */
  lazy val laws: Laws[InvolutionEqual] =
    ExcludedMiddleLaws.laws + NoncontradictionLaws.laws + involutionLaw

}
