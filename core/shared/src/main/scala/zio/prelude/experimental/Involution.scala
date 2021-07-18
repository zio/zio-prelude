package zio.prelude
package experimental

import zio.prelude.experimental.coherent.InvolutionEqual
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait Involution[A] extends ComplementShape[A]

object Involution extends Lawful[InvolutionEqual] {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Involution[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

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
    involutionLaw

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    involution: Involution.Aux[A, Join, Meet]
  ): Involution.Aux[A, Join, Meet] =
    involution
}
