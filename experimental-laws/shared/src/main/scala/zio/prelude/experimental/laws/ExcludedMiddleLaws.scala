package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object ExcludedMiddleLaws extends Lawful[ExcludedMiddleEqual] {

  /**
   * The excluded middle law states that for the join operator `vvv`, the top element `1`, complement operator `!`
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !a vvv a === 1
   * }}}
   */
  lazy val excludedMiddleLaw: Laws[ExcludedMiddleEqual] =
    new Laws.Law1[ExcludedMiddleEqual]("excludedMiddleLaw") {
      def apply[A](a: A)(implicit A: ExcludedMiddleEqual[A]): TestResult =
        (!a vvv a) <-> A.top
    }

  /**
   * The set of all laws that instances of `ExcludedMiddle` must satisfy.
   */
  lazy val laws: Laws[ExcludedMiddleEqual] =
    AbsorptionLaws.laws + excludedMiddleLaw

}
