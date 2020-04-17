package zio

import zio.test.{ assert, TestResult }

package object prelude
    extends Assertions
    with AssociativeFSyntax
    with ClosureSyntax
    with CovariantSyntax
    with ContravariantSyntax
    with DebugSyntax
    with EqualSyntax
    with HashSyntax
    with IdExports
    with IdentitySyntax
    with NewtypeExports
    with NewtypeFExports
    with OrdSyntax {

  type <=>[A, B] = Equivalence[A, B]

  object classic {
    type Semigroup[A]            = Associative[A]
    type CommutativeSemigroup[A] = Semigroup[A] with Commutative[A]
    type Monoid[A]               = Semigroup[A] with Identity[A]
    type CommutativeMonoid[A]    = Monoid[A] with Commutative[A]

    type Functor[F[+_]]       = Covariant[F]
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]
    type Invariant[F[_]]      = zio.prelude.Invariant[F]

    type Apply[F[+_]] = Covariant[F] with AssociativeF.Both[F]
  }

  /**
   * Provides implicit syntax for assertions.
   */
  implicit class AssertionSyntax[A](private val self: A) extends AnyVal {
    def <->[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult =
      equal(that)
    def equal[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult =
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
