/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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

import zio.prelude.newtypes.Natural

package object prelude
    extends AssociativeSyntax
    with AssociativeBothSyntax
    with AssociativeComposeSyntax
    with AssociativeEitherSyntax
    with AssociativeFlattenSyntax
    with BicovariantSyntax
    with CommutativeBothSyntax
    with CommutativeEitherSyntax
    with ConstExports
    with ContravariantSyntax
    with CovariantSyntax
    with DebugSyntax
    with DivariantSyntax
    with EqualSyntax
    with ForEachSyntax
    with HashSyntax
    with IdExports
    with IdentityBothSyntax
    with IdentityEitherSyntax
    with IdentitySyntax
    with InvariantSyntax
    with InverseSyntax
    with NewtypeFExports
    with NonEmptyForEachSyntax
    with NonEmptyListSyntax
    with NonEmptySetSyntax
    with OrdSyntax
    with PartialOrdSyntax
    with ZNonEmptySetSyntax
    with ZSetSyntax
    with ZivariantSyntax
    with CovariantFilterSyntax
    with ForEachFilterSyntax {

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
}
