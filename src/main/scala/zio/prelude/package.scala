package zio

import zio.test.{ assert, TestResult }

package object prelude
    extends DebugSyntax
    with EqualSyntax
    with OrdSyntax
    with HashSyntax
    with ClosureSyntax
    with NewtypeExports
    with NewtypeFExports
    with IdentitySyntax
    with Newtypes
    with Assertions {

  /**
   * Provides implicit syntax for assertions.
   */
  implicit class AssertionSyntax[A](private val self: A) extends AnyVal {
    def <->(that: A)(implicit eq: Equal[A]): TestResult =
      assert(self)(equalTo(that))
    def greaterThan(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isGreaterThan(that))
    def greaterThanEqualTo(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isGreaterThanEqualTo(that))
    def lessThan(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isLessThan(that))
    def lessThanEqualTo(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isLessThanEqualTo(that))
  }
}
