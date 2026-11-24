package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `Noncontradiction` instances must satisfy, checked with `!`, `^^^`,
 * `bottom` and `Equal`. They include the `AbsorptionLaws`. They belong to the
 * experimental module, so they can change between releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(NoncontradictionLaws)(Gen.boolean)
 * // succeeds with a passing TestResult
 * }}}
 */
object NoncontradictionLaws extends Lawful[NoncontradictionEqual] {

  /**
   * The non-contradiction law states that for the meet operator `^^^`, the
   * `bottom` element and the complement operator `!`, for all values `a`, the
   * following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * !a ^^^ a === bottom
   * }}}
   */
  lazy val noncontradictionLaw: Laws[NoncontradictionEqual] =
    new Laws.Law1[NoncontradictionEqual]("noncontradictionLaw") {
      def apply[A](a: A)(implicit A: NoncontradictionEqual[A]): TestResult =
        (!a ^^^ a) <-> A.bottom
    }

  /**
   * The set of all laws that instances of `Noncontradiction` must satisfy:
   * the `AbsorptionLaws` and the non-contradiction law.
   */
  lazy val laws: Laws[NoncontradictionEqual] =
    AbsorptionLaws.laws + noncontradictionLaw

}
