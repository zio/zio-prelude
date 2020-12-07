package zio.prelude

import zio.prelude.coherent.InvolutionEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Involution[A, +Join[x] <: Associative[x], +Meet[x] <: Associative[x]] extends ComplementShape[A, Join, Meet]

object Involution extends Lawful[InvolutionEqual] {

  /**
   * The join involution law states that for the complement operator `!`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !(!a) === a
   * }}}
   */
  val involutionLaw: Laws[InvolutionEqual] =
    new Laws.Law1[InvolutionEqual]("involutionLaw") {
      def apply[A](a: A)(implicit A: InvolutionEqual[A]): TestResult =
        A.complement(A.complement(a)) <-> a
    }

  /**
   * The set of all laws that instances of `Involution` must satisfy.
   */
  val laws: Laws[InvolutionEqual] =
    involutionLaw

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    involution: Involution[A, Join, Meet]
  ): Involution[A, Join, Meet] =
    involution
}
