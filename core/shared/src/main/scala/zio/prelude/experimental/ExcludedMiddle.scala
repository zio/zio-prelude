package zio.prelude
package experimental

import zio.prelude.experimental.coherent.ExcludedMiddleEqual
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait ExcludedMiddle[A] extends TopShape[A] with ComplementShape[A]

object ExcludedMiddle extends Lawful[ExcludedMiddleEqual] {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Identity[x]] = ExcludedMiddle[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

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
    excludedMiddleLaw

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Identity[x]](implicit
    excludedMiddle: ExcludedMiddle.Aux[A, Join, Meet]
  ): ExcludedMiddle.Aux[A, Join, Meet] =
    excludedMiddle
}
