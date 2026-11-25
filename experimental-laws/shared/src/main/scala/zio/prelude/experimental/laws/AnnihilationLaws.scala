package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

/**
 * Laws that `Annihilation` instances must satisfy, checked with `***`,
 * `annihilation` and `Equal`. They include the `DistributiveProdLaws`. They
 * belong to the experimental module, so they can change between releases.
 *
 * {{{
 * import zio.test._
 * import zio.test.laws._
 *
 * val check = checkAllLaws(AnnihilationLaws)(Gen.int)
 * // succeeds with a passing TestResult
 * }}}
 */
object AnnihilationLaws extends Lawful[AnnihilationEqual] {

  /**
   * The left annihilation law states that for the multiplication operator
   * `***`, the `annihilation` element (by default the identity of addition)
   * and for all values `a`, the following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * annihilation *** a === annihilation
   * }}}
   */
  lazy val leftAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("leftAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (A.annihilation *** a) <-> A.annihilation
    }

  /**
   * The right annihilation law states that for the multiplication operator
   * `***`, the `annihilation` element (by default the identity of addition)
   * and for all values `a`, the following must hold:
   *
   * {{{
   * // no-check: algebraic notation, not Scala code
   * a *** annihilation === annihilation
   * }}}
   */
  lazy val rightAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("rightAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (a *** A.annihilation) <-> A.annihilation
    }

  /**
   * The set of all laws that instances of `Annihilation` must satisfy: the
   * `DistributiveProdLaws` and the two annihilation laws.
   */
  lazy val laws: Laws[AnnihilationEqual] =
    DistributiveProdLaws.laws + leftAnnihilationLaw + rightAnnihilationLaw

}
