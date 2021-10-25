package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object AnnihilationLaws extends Lawful[AnnihilationEqual] {

  /**
   * The left annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * 0 * a === 0
   * }}}
   */
  lazy val leftAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("leftAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (A.annihilation *** a) <-> A.annihilation
    }

  /**
   * The right annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * a * 0 === 0
   * }}}
   */
  lazy val rightAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("rightAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (a *** A.annihilation) <-> A.annihilation
    }

  /**
   * The set of all laws that instances of `Annihilation` must satisfy.
   */
  lazy val laws: Laws[AnnihilationEqual] =
    leftAnnihilationLaw + rightAnnihilationLaw

}
