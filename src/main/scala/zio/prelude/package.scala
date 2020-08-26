package zio

import com.github.ghik.silencer.silent
import zio.test.{ assert, TestResult }

package object prelude
    extends Assertions
    with AssociativeSyntax
    with AssociativeBothSyntax
    with AssociativeComposeSyntax
    with AssociativeEitherSyntax
    with AssociativeFlattenSyntax
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
    with InverseSyntax
    with NewtypeExports
    with NewtypeFExports
    with NonEmptyTraversableSyntax
    with OrdSyntax
    with TraversableSyntax {

  type <=>[A, B] = Equivalence[A, B]

  type AnyF[_] = Any

  type EState[S, +E, +A] = zio.prelude.fx.ZPure[S, S, Any, E, A]
  val EState: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type State[S, +A] = zio.prelude.fx.ZPure[S, S, Any, Nothing, A]
  val State: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type Reader[-R, +A] = zio.prelude.fx.ZPure[Unit, Unit, R, Nothing, A]
  val Reader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type EReader[-R, +E, +A] = zio.prelude.fx.ZPure[Unit, Unit, R, E, A]
  val EReader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type MultiSet[+A] = ZSet[A, Int]
  val MultiSet: ZSet.type = ZSet

  type DeriveAssociative[F[_]] = Derive[F, Associative]
  type DeriveCommutative[F[_]] = Derive[F, Commutative]
  type DeriveDebug[F[_]]       = Derive[F, Debug]
  type DeriveEqual[F[_]]       = Derive[F, Equal]
  type DeriveHash[F[_]]        = Derive[F, Hash]
  type DeriveIdentity[F[_]]    = Derive[F, Identity]
  type DeriveInverse[F[_]]     = Derive[F, Inverse]
  type DeriveOrd[F[_]]         = Derive[F, Ord]

  object classic {
    type Semigroup[A]            = Associative[A]
    type CommutativeSemigroup[A] = Commutative[A]
    type Monoid[A]               = Identity[A]
    type CommutativeMonoid[A]    = Commutative[A] with Identity[A]
    type Group[A]                = Inverse[A]
    type AbelianGroup[A]         = Commutative[A] with Inverse[A]

    type Functor[F[+_]]       = Covariant[F]
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]
    type Invariant[F[_]]      = zio.prelude.Invariant[F]
    type Alternative[F[+_]] =
      Covariant[F] with IdentityBoth[F] with IdentityEither[F]
    type InvariantAlt[F[_]] =
      Invariant[F] with IdentityBoth[F] with IdentityEither[F]

    type InvariantSemigroupal[F[_]]      = Invariant[F] with AssociativeBoth[F]
    type Semigroupal[F[+_]]              = Covariant[F] with AssociativeBoth[F]
    type ContravariantSemigroupal[F[-_]] = Contravariant[F] with AssociativeBoth[F]

    type SemigroupK[F[_]] = AssociativeEither[F]
    type MonoidK[F[_]]    = IdentityEither[F]

    type ContravariantMonoidal[F[-_]] = Contravariant[F] with IdentityBoth[F]
    type InvariantMonoidal[F[_]]      = Invariant[F] with IdentityBoth[F]

    type FlatMap[F[+_]] = Covariant[F] with AssociativeFlatten[F]
    type Monad[F[+_]]   = Covariant[F] with IdentityFlatten[F]

    type Divide[F[-_]]    = Contravariant[F] with AssociativeBoth[F]
    type Divisible[F[-_]] = Contravariant[F] with IdentityBoth[F]
    type Decidable[F[-_]] =
      Contravariant[F] with IdentityBoth[F] with IdentityEither[F]

    type Apply[F[+_]]               = Covariant[F] with AssociativeBoth[F]
    type Applicative[F[+_]]         = Covariant[F] with IdentityBoth[F]
    type InvariantApplicative[F[_]] = Invariant[F] with IdentityBoth[F]

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

  implicit class AnySyntax[A](private val a: A) extends AnyVal {

    @silent("side-effecting nullary methods are discouraged")
    /* Ignores the value, if you explicitly want to do so and avoids "Unused value" compiler warnings. */
    def ignore: Unit = ()

    /** Applies function `f` to a value `a`, like `f(a)`, but in flipped order and doesn't need parentheses. Can be chained, like `x |> f |> g`. */
    def |>[B](f: A => B): B = f(a)

    /** Applies the function `f` to the value `a`, ignores the result, and returns the original value `a`. Practical for debugging, like `x.someMethod.tee(println(_)).someOtherMethod...` . Similar to the `tee` UNIX command. */
    def tap(f: A => Any): A = {
      val _ = f(a)
      a
    }

  }

}
