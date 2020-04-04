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
    with Assertions {

  object classic {
    type Semigroup[A]            = Associative[A]
    type CommutativeSemigroup[A] = Semigroup[A] with Commutative[A]
    type Monoid[A]               = Semigroup[A] with Identity[A]
    type CommutativeMonoid[A]    = Monoid[A] with Commutative[A]
  }

  /**
   * Provides implicit syntax for assertions.
   */
  implicit class AssertionSyntax[A](private val self: A) extends AnyVal {
    def <->(that: A)(implicit eq: Equal[A]): TestResult =
      equal(that)
    def equal(that: A)(implicit eq: Equal[A]): TestResult =
      assert(self)(equalTo(that))
    def greater(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isGreaterThan(that))
    def greaterOrEqual(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isGreaterThanEqualTo(that))
    def less(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isLessThan(that))
    def lessOrEqual(that: A)(implicit ord: Ord[A]): TestResult =
      assert(self)(isLessThanEqualTo(that))
  }
}
