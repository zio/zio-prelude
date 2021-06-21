/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio

import com.github.ghik.silencer.silent
import zio.prelude.newtypes._
import zio.test.{TestResult, assert}

package object prelude
    extends Assertions
    with AssociativeSyntax
    with AssociativeBothSyntax
    with AssociativeComposeSyntax
    with AssociativeEitherSyntax
    with AssociativeFlattenSyntax
    with BicovariantSyntax
    with CommutativeBothSyntax
    with CommutativeEitherSyntax
    with ConstExports
    with CovariantSyntax
    with ContravariantSyntax
    with DebugSyntax
    with DivariantSyntax
    with EqualSyntax
    with HashSyntax
    with IdExports
    with IdentitySyntax
    with IdentityBothSyntax
    with IdentityEitherSyntax
    with InvariantSyntax
    with InverseSyntax
    with NewtypeExports
    with NewtypeFExports
    with NonEmptyListSyntax
    with NonEmptySetSyntax
    with NonEmptyForEachSyntax
    with OrdSyntax
    with PartialOrdSyntax
    with ForEachSyntax
    with ZNonEmptySetSyntax
    with ZSetSyntax
    with ZivariantSyntax {

  type <=>[A, B] = Equivalence[A, B]

  type AnyF[_] = Any

  type EState[S, +E, +A] = zio.prelude.fx.ZPure[Nothing, S, S, Any, E, A]
  val EState: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type State[S, +A] = zio.prelude.fx.ZPure[Nothing, S, S, Any, Nothing, A]
  val State: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type Reader[-R, +A] = zio.prelude.fx.ZPure[Nothing, Unit, Unit, R, Nothing, A]
  val Reader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type EReader[-R, +E, +A] = zio.prelude.fx.ZPure[Nothing, Unit, Unit, R, E, A]
  val EReader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type Writer[+W, +A] = zio.prelude.fx.ZPure[W, Unit, Unit, Any, Nothing, A]
  val Writer: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type EWriter[+W, +E, +A] = zio.prelude.fx.ZPure[W, Unit, Unit, Any, E, A]
  val EWriter: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  type Validation[+E, +A] = ZValidation[Nothing, E, A]
  val Validation: ZValidation.type = ZValidation

  type MultiSet[+A] = ZSet[A, Natural]
  val MultiSet: ZSet.type = ZSet
  type NonEmptyMultiSet[+A] = ZNonEmptySet[A, Natural]
  val NonEmptyMultiSet: ZNonEmptySet.type = ZNonEmptySet

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

    type Semilattice[A]        = Commutative[A] with Idempotent[A]
    type BoundedSemilattice[A] = Semilattice[A] with Identity[A]

    type Functor[F[+_]]       = Covariant[F]
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]
    type Invariant[F[_]]      = zio.prelude.Invariant[F]
    type Alternative[F[+_]]   =
      Covariant[F] with IdentityBoth[F] with IdentityEither[F]
    type InvariantAlt[F[_]]   =
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

    type Category[=>:[-_, +_]]   = IdentityCompose[=>:]
    type Profunctor[=>:[-_, +_]] = Divariant[=>:]
    type Bifunctor[=>:[+_, +_]]  = Bicovariant[=>:]
  }

  /**
   * Provides implicit syntax for assertions.
   */
  implicit class AssertionSyntax[A](private val self: A) extends AnyVal {
    def <->[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult       =
      isEqualTo(that)
    // name intentionally different from other methods (`equal`, `equalTo`, etc to avoid confusing compiler errors)
    def isEqualTo[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult =
      assert(self)(equalTo(that))
    def greater(that: A)(implicit ord: PartialOrd[A]): TestResult        =
      assert(self)(isGreaterThan(that))
    def greaterOrEqual(that: A)(implicit ord: PartialOrd[A]): TestResult =
      assert(self)(isGreaterThanEqualTo(that))
    def less(that: A)(implicit ord: PartialOrd[A]): TestResult           =
      assert(self)(isLessThan(that))
    def lessOrEqual(that: A)(implicit ord: PartialOrd[A]): TestResult    =
      assert(self)(isLessThanEqualTo(that))
  }

  implicit class MapSyntax[K, V](private val l: Map[K, V]) extends AnyVal {

    /** Compares two maps, where you supply `compareValues` that compares the common values */
    def compareWith(
      compareValues: (Ordering, Iterable[(V, V)]) => PartialOrdering
    )(r: Map[K, V]): PartialOrdering = {
      def commonValues(lesserMap: Map[K, V]): Iterable[(V, V)] =
        lesserMap.keys.map(k => (l(k), r(k)))
      if (l.keySet == r.keySet) {
        compareValues(Ordering.Equals, commonValues(l))
      } else if (l.keySet.subsetOf(r.keySet)) {
        compareValues(Ordering.LessThan, commonValues(l))
      } else if (r.keySet.subsetOf(l.keySet)) {
        compareValues(Ordering.GreaterThan, commonValues(r))
      } else {
        PartialOrdering.Incomparable
      }
    }

    /** Compares two maps, allowing for the values to be lesser in the lesser map or greater in the greater map */
    def compareSoft(r: Map[K, V])(implicit V: PartialOrd[V]): PartialOrdering = {
      def compareValues(expected: Ordering, commonValues: Iterable[(V, V)]): PartialOrdering =
        commonValues.foldLeft[PartialOrdering](expected) { case (acc, (l, r)) => acc.unify(l =??= r) }
      compareWith(compareValues)(r)
    }

    /** Compares two maps, expecting the values for the common keys to be equal. */
    def compareStrict(r: Map[K, V])(implicit V: Equal[V]): PartialOrdering = {
      def compareValues(expected: Ordering, commonValues: Iterable[(V, V)]): PartialOrdering =
        if (commonValues.forall { case (l, r) => l === r }) {
          expected
        } else {
          PartialOrdering.Incomparable
        }
      compareWith(compareValues)(r)
    }
  }

  val any: Any = ()

  implicit final class AnySyntax[A](private val a: A) extends AnyVal {

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
