package zio.prelude.laws

import zio.prelude.coherent.EqualPartialInverse
import zio.test.laws._
import zio.test.{TestResult, assertCompletes}

object PartialInverseLaws extends Lawful[EqualPartialInverse] {

  /**
   * The partial inverse law states that for some binary operator `*`,
   * for all values `a`, if the operation is defined, the following must hold:
   *
   * {{{
   * a * a === identity
   * }}}
   */
  lazy val partialInverseLaw: Laws[EqualPartialInverse] =
    new Laws.Law1[EqualPartialInverse]("rightPartialInverseLaw") {
      def apply[A](a: A)(implicit I: EqualPartialInverse[A]): TestResult =
        I.inverseOption(a, a) match {
          case Some(a) => a <-> I.identity
          case None    => assertCompletes
        }
    }

  /**
   * The set of all laws that instances of `PartialInverse` must satisfy.
   */
  lazy val laws: Laws[EqualPartialInverse] =
    IdentityLaws.laws + partialInverseLaw

}
