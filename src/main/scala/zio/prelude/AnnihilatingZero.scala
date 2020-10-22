package zio.prelude

import zio.prelude.coherent.AnnihilatingZeroEqual
import zio.prelude.newtypes.Sum
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait AnnihilatingZero[A, +Addition[x] <: Identity[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiply[A, Addition, Multiplication]

object AnnihilatingZero extends Lawful[AnnihilatingZeroEqual] {

  /**
   * The left annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * 0 * a === 0
   * }}}
   */
  val leftAnnihilationLaw: Laws[AnnihilatingZeroEqual] =
    new Laws.Law1[AnnihilatingZeroEqual]("leftAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilatingZeroEqual[A]): TestResult =
        (Sum.unwrap(A.Addition.identity) *** a) <-> Sum.unwrap(A.Addition.identity)
    }

  /**
   * The right annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * a * 0 === 0
   * }}}
   */
  val rightAnnihilationLaw: Laws[AnnihilatingZeroEqual] =
    new Laws.Law1[AnnihilatingZeroEqual]("rightAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilatingZeroEqual[A]): TestResult =
        (a *** Sum.unwrap(A.Addition.identity)) <-> Sum.unwrap(A.Addition.identity)
    }

  /**
   * The set of all laws that instances of `AnnihilatingZero` must satisfy.
   */
  val laws: Laws[AnnihilatingZeroEqual] =
    leftAnnihilationLaw + rightAnnihilationLaw

  /**
   * Summons an implicit `AnnihilatingZero[A]`.
   */
  def apply[A, Addition[x] <: Identity[x], Multiplication[x] <: Associative[x]](implicit
    AnnihilatingZero: AnnihilatingZero[A, Addition, Multiplication]
  ): AnnihilatingZero[A, Addition, Multiplication] =
    AnnihilatingZero
}
