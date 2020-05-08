package zio

import zio.test.{ assert, TestResult }

package object prelude
    extends Assertions
    with AssociativeBothFSyntax
    with AssociativeEitherFSyntax
    with ClosureSyntax
    with CommutativeBothFSyntax
    with CommutativeEitherFSyntax
    with CovariantSyntax
    with ContravariantSyntax
    with DebugSyntax
    with EqualSyntax
    with HashSyntax
    with IdExports
    with IdentitySyntax
    with IdentityBothFSyntax
    with IdentityEitherFSyntax
    with NewtypeExports
    with NewtypeFExports
    with OrdSyntax {

  type <=>[A, B] = Equivalence[A, B]

  type AnyF[_] = Any

  object classic {
    type Semigroup[A]            = Associative[A]
    type CommutativeSemigroup[A] = Associative[A] with Commutative[A]
    type Monoid[A]               = Associative[A] with Identity[A]
    type CommutativeMonoid[A]    = Associative[A] with Commutative[A] with Identity[A]
    type Group[A]                = Associative[A] with Identity[A] with Inverse[A]
    type AbelianGroup[A]         = Associative[A] with Commutative[A] with Identity[A] with Inverse[A]

    type Functor[F[+_]]       = Covariant[F]
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]
    type Invariant[F[_]]      = zio.prelude.Invariant[F]
    type Alternative[F[+_]] =
      Covariant[F] with AssociativeBothF[F] with IdentityBothF[F] with AssociativeEitherF[F] with IdentityEitherF[F]
    type InvariantAlt[F[_]] =
      Invariant[F] with AssociativeBothF[F] with IdentityBothF[F] with AssociativeEitherF[F] with IdentityEitherF[F]

    type InvariantSemigroupal[F[_]]      = Invariant[F] with AssociativeBothF[F]
    type Semigroupal[F[+_]]              = Covariant[F] with AssociativeBothF[F]
    type ContravariantSemigroupal[F[-_]] = Contravariant[F] with AssociativeBothF[F]

    type SemigroupK[F[_]] = AssociativeEitherF[F]
    type MonoidK[F[_]]    = AssociativeEitherF[F] with IdentityEitherF[F]

    type ContravariantMonoidal[F[-_]] = Contravariant[F] with AssociativeBothF[F] with IdentityBothF[F]
    type InvariantMonoidal[F[_]]      = Invariant[F] with AssociativeBothF[F] with IdentityBothF[F]

    type FlatMap[F[+_]] = Covariant[F] with AssociativeFlatten[F]
    type Monad[F[+_]]   = Covariant[F] with AssociativeFlatten[F] with AssociativeBothF[F] // Close but not quite

    type Divide[F[-_]]    = Contravariant[F] with AssociativeBothF[F]
    type Divisible[F[-_]] = Contravariant[F] with AssociativeBothF[F] with IdentityBothF[F]
    type Decidable[F[-_]] =
      Contravariant[F] with AssociativeBothF[F] with IdentityBothF[F] with AssociativeEitherF[F] with IdentityEitherF[F]

    type Apply[F[+_]]               = Covariant[F] with AssociativeBothF[F]
    type Applicative[F[+_]]         = Covariant[F] with AssociativeBothF[F] with IdentityBothF[F]
    type InvariantApplicative[F[_]] = Invariant[F] with AssociativeBothF[F] with IdentityBothF[F]
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
