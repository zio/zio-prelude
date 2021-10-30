package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object NoncontradictionLaws extends Lawful[NoncontradictionEqual] {

  /**
   * The non-contradiction law states that for the meet operator `^^^`, the bottom element `0`, complement operator `!`
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !a ^^^ a === 0
   * }}}
   */
  lazy val noncontradictionLaw: Laws[NoncontradictionEqual] =
    new Laws.Law1[NoncontradictionEqual]("noncontradictionLaw") {
      def apply[A](a: A)(implicit A: NoncontradictionEqual[A]): TestResult =
        (!a ^^^ a) <-> A.bottom
    }

  /**
   * The set of all laws that instances of `Noncontradiction` must satisfy.
   */
  lazy val laws: Laws[NoncontradictionEqual] =
    AbsorptionLaws.laws + noncontradictionLaw

}
