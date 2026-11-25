package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `ExcludedMiddle` instances must satisfy, checked with `!`, `vvv`,
 * `top` and `Equal`. They include the `AbsorptionLaws`. They belong to the
 * experimental module, so they can change between releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(ExcludedMiddleLaws)(Gen.boolean)
 * // succeeds with a passing TestResult
 * }}}
 */
object ExcludedMiddleLaws extends Lawful[ExcludedMiddleEqual] {

  /**
   * The excluded middle law states that for the join operator `vvv`, the
   * `top` element and the complement operator `!`, for all values `a`, the
   * following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * !a vvv a === top
   * }}}
   */
  lazy val excludedMiddleLaw: Laws[ExcludedMiddleEqual] =
    new Laws.Law1[ExcludedMiddleEqual]("excludedMiddleLaw") {
      def apply[A](a: A)(implicit A: ExcludedMiddleEqual[A]): TestResult =
        (!a vvv a) <-> A.top
    }

  /**
   * The set of all laws that instances of `ExcludedMiddle` must satisfy: the
   * `AbsorptionLaws` and the excluded middle law.
   */
  lazy val laws: Laws[ExcludedMiddleEqual] =
    AbsorptionLaws.laws + excludedMiddleLaw

}
