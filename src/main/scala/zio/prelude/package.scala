package zio

import zio.test.{ assert, TestResult }

package object prelude
    extends Assertions
    with AssociativeBothSyntax
    with AssociativeEitherSyntax
    with AssociativeFlattenSyntax
    with ClosureSyntax
    with CommutativeBothSyntax
    with CommutativeEitherSyntax
    with CovariantSyntax
    with ContravariantSyntax
    with DebugSyntax
    with EqualSyntax
    with HashSyntax
    with IdExports
    with IdentitySyntax
    with IdentityBothSyntax
    with IdentityEitherSyntax
    with NewtypeExports
    with NewtypeFExports
    with OrdSyntax {

  type <=>[A, B] = Equivalence[A, B]

  type AnyF[_] = Any

  object classic {
    type Semigroup[A]            = Associative[A]
    type CommutativeSemigroup[A] = Semigroup[A] with Commutative[A]
    type Monoid[A]               = Semigroup[A] with Identity[A]
    type CommutativeMonoid[A]    = Monoid[A] with Commutative[A]
    type Group[A]                = Monoid[A] with Inverse[A]
    type AbelianGroup[A]         = Group[A] with Commutative[A]

    type Functor[F[+_]]       = Covariant[F]
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]
    type Invariant[F[_]]      = zio.prelude.Invariant[F]
    type Alternative[F[+_]]   = Applicative[F] with MonoidK[F]
    type InvariantAlt[F[_]]   = InvariantApplicative[F] with MonoidK[F]

    type InvariantSemigroupal[F[_]]      = Invariant[F] with AssociativeBoth[F]
    type Semigroupal[F[+_]]              = Covariant[F] with InvariantSemigroupal[F]
    type ContravariantSemigroupal[F[-_]] = Contravariant[F] with InvariantSemigroupal[F]

    type SemigroupK[F[_]] = AssociativeEither[F]
    type MonoidK[F[_]]    = SemigroupK[F] with IdentityEither[F]

    type ContravariantMonoidal[F[-_]] = Contravariant[F] with AssociativeBoth[F] with IdentityBoth[F]
    type InvariantMonoidal[F[_]]      = Invariant[F] with AssociativeBoth[F] with IdentityBoth[F]

    type FlatMap[F[+_]] = Covariant[F] with AssociativeFlatten[F]
    type Monad[F[+_]]   = Covariant[F] with AssociativeFlatten[F] with AssociativeBoth[F] // Close but not quite

    type Divide[F[-_]]    = Contravariant[F] with AssociativeBoth[F]
    type Divisible[F[-_]] = Divide[F] with IdentityBoth[F] // with IdentityEither[F]
    type Decidable[F[-_]] = Divisible[F] with AssociativeEither[F]

    type Apply[F[+_]]               = Covariant[F] with AssociativeBoth[F]
    type Applicative[F[+_]]         = Covariant[F] with AssociativeBoth[F] with IdentityBoth[F]
    type InvariantApplicative[F[_]] = Invariant[F] with AssociativeBoth[F] with IdentityBoth[F]
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
