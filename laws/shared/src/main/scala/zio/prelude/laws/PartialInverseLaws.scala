package zio.prelude.laws

import zio.prelude.coherent.EqualPartialInverse
import zio.test.laws._
import zio.test.{TestResult, assertCompletes}

/**
 * Laws that instances of `PartialInverse` must satisfy. They include the
 * `Identity` laws.
 */
object PartialInverseLaws extends Lawful[EqualPartialInverse] {

  /**
   * The partial inverse law states that for all values `a`, when
   * `inverseOption(a, a)` returns `Some(x)`, `x` must equal the identity. When
   * it returns `None`, the law only checks that the call completes.
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
