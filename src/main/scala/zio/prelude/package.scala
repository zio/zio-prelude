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

  type EState[S, +E, +A] = ZPure[S, S, Any, E, A]
  val EState: ZPure.type = ZPure

  type State[S, +A] = ZPure[S, S, Any, Nothing, A]
  val State: ZPure.type = ZPure

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
      Covariant[F] with AssociativeBoth[F] with IdentityBoth[F] with AssociativeEither[F] with IdentityEither[F]
    type InvariantAlt[F[_]] =
      Invariant[F] with AssociativeBoth[F] with IdentityBoth[F] with AssociativeEither[F] with IdentityEither[F]

    type InvariantSemigroupal[F[_]]      = Invariant[F] with AssociativeBoth[F]
    type Semigroupal[F[+_]]              = Covariant[F] with AssociativeBoth[F]
    type ContravariantSemigroupal[F[-_]] = Contravariant[F] with AssociativeBoth[F]

    type SemigroupK[F[_]] = AssociativeEither[F]
    type MonoidK[F[_]]    = AssociativeEither[F] with IdentityEither[F]

    type ContravariantMonoidal[F[-_]] = Contravariant[F] with AssociativeBoth[F] with IdentityBoth[F]
    type InvariantMonoidal[F[_]]      = Invariant[F] with AssociativeBoth[F] with IdentityBoth[F]

    type FlatMap[F[+_]] = Covariant[F] with AssociativeFlatten[F]
    type Monad[F[+_]]   = Covariant[F] with IdentityFlatten[F]

    type Divide[F[-_]]    = Contravariant[F] with AssociativeBoth[F]
    type Divisible[F[-_]] = Contravariant[F] with AssociativeBoth[F] with IdentityBoth[F]
    type Decidable[F[-_]] =
      Contravariant[F] with AssociativeBoth[F] with IdentityBoth[F] with AssociativeEither[F] with IdentityEither[F]

    type Apply[F[+_]]               = Covariant[F] with AssociativeBoth[F]
    type Applicative[F[+_]]         = Covariant[F] with AssociativeBoth[F] with IdentityBoth[F]
    type InvariantApplicative[F[_]] = Invariant[F] with AssociativeBoth[F] with IdentityBoth[F]

    type Category[:=>[-_, +_]]   = IdentityCompose[:=>]
    type Profunctor[:=>[-_, +_]] = Divariant[:=>]
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
