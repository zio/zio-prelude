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
    with IdentityFlattenSyntax
    with IdentitySyntax
    with InvariantSyntax
    with InverseSyntax
    with NewtypeFExports
    with NonEmptyForEachSyntax
    with NonEmptyListSyntax
    with NonEmptySetSyntax
    with OrdSyntax
    with PartialInverseSyntax
    with PartialOrdSyntax
    with ZNonEmptySetSyntax
    with ZSetSyntax
    with ZivariantSyntax {

  /**
   * A symbolic alias for `Equivalence[A, B]`, a pair of functions `A => B` and `B => A`.
   */
  type <=>[A, B] = Equivalence[A, B]

  /**
   * A type constructor that ignores its type argument and is always `Any`.
   */
  type AnyF[_] = Any

  /**
   * A `ZPure` computation that reads and updates a state of type `S` and either fails with an `E` or succeeds with an
   * `A`. It writes no log and needs no environment.
   */
  type EState[S, +E, +A] = zio.prelude.fx.ZPure[Nothing, S, S, Any, E, A]

  /**
   * The `ZPure` companion object under the name `EState`, for constructing `EState` computations.
   */
  val EState: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZPure` computation that reads and updates a state of type `S` and succeeds with an `A`. It cannot fail, writes
   * no log and needs no environment.
   *
   * {{{
   * import zio.prelude._
   *
   * val counter: State[Int, String] =
   *   State.update[Int, Int](_ + 1) *> State.get[Int].map(n => s"count: \$n")
   *
   * val result: (Int, String) = counter.run(41)
   * // (42, "count: 42")
   * }}}
   */
  type State[S, +A] = zio.prelude.fx.ZPure[Nothing, S, S, Any, Nothing, A]

  /**
   * The `ZPure` companion object under the name `State`, for constructing `State` computations.
   */
  val State: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZPure` computation that reads an environment of services of type `R` and succeeds with an `A`. Its state is
   * `Unit`, and it cannot fail and writes no log.
   */
  type Reader[-R, +A] = zio.prelude.fx.ZPure[Nothing, Unit, Unit, R, Nothing, A]

  /**
   * The `ZPure` companion object under the name `Reader`, for constructing `Reader` computations.
   */
  val Reader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZPure` computation that reads an environment of services of type `R` and either fails with an `E` or succeeds
   * with an `A`. Its state is `Unit`, and it writes no log.
   */
  type EReader[-R, +E, +A] = zio.prelude.fx.ZPure[Nothing, Unit, Unit, R, E, A]

  /**
   * The `ZPure` companion object under the name `EReader`, for constructing `EReader` computations.
   */
  val EReader: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZPure` computation that writes log entries of type `W` and succeeds with an `A`. Its state is `Unit`, and it
   * cannot fail and needs no environment.
   */
  type Writer[+W, +A] = zio.prelude.fx.ZPure[W, Unit, Unit, Any, Nothing, A]

  /**
   * The `ZPure` companion object under the name `Writer`, for constructing `Writer` computations.
   */
  val Writer: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZPure` computation that writes log entries of type `W` and either fails with an `E` or succeeds with an `A`. Its
   * state is `Unit`, and it needs no environment.
   */
  type EWriter[+W, +E, +A] = zio.prelude.fx.ZPure[W, Unit, Unit, Any, E, A]

  /**
   * The `ZPure` companion object under the name `EWriter`, for constructing `EWriter` computations.
   */
  val EWriter: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure

  /**
   * A `ZValidation` without warnings: it either fails with one or more errors of type `E` or succeeds with an `A`.
   */
  type Validation[+E, +A] = ZValidation[Nothing, E, A]

  /**
   * The `ZValidation` companion object under the name `Validation`, for constructing `Validation` values.
   */
  val Validation: ZValidation.type = ZValidation

  /**
   * A `ZSet` whose measure is a `Natural` number, that is, a set that counts how many times each element occurs.
   */
  type MultiSet[+A] = ZSet[A, Natural]

  /**
   * The `ZSet` companion object under the name `MultiSet`, for constructing `MultiSet` values.
   */
  val MultiSet: ZSet.type = ZSet

  /**
   * A `ZNonEmptySet` whose measure is a `Natural` number, that is, a non-empty set that counts how many times each
   * element occurs.
   */
  type NonEmptyMultiSet[+A] = ZNonEmptySet[A, Natural]

  /**
   * The `ZNonEmptySet` companion object under the name `NonEmptyMultiSet`, for constructing `NonEmptyMultiSet` values.
   */
  val NonEmptyMultiSet: ZNonEmptySet.type = ZNonEmptySet

  /**
   * A `Derive` for `Associative`: derives an `Associative[F[A]]` from an `Associative[A]`.
   */
  type DeriveAssociative[F[_]] = Derive[F, Associative]

  /**
   * A `Derive` for `Commutative`: derives a `Commutative[F[A]]` from a `Commutative[A]`.
   */
  type DeriveCommutative[F[_]] = Derive[F, Commutative]

  /**
   * A `Derive` for `Debug`: derives a `Debug[F[A]]` from a `Debug[A]`.
   */
  type DeriveDebug[F[_]] = Derive[F, Debug]

  /**
   * A `Derive` for `Equal`: derives an `Equal[F[A]]` from an `Equal[A]`.
   */
  type DeriveEqual[F[_]] = Derive[F, Equal]

  /**
   * A `Derive` for `Hash`: derives a `Hash[F[A]]` from a `Hash[A]`.
   */
  type DeriveHash[F[_]] = Derive[F, Hash]

  /**
   * A `Derive` for `Identity`: derives an `Identity[F[A]]` from an `Identity[A]`.
   */
  type DeriveIdentity[F[_]] = Derive[F, Identity]

  /**
   * A `Derive` for `Inverse`: derives an `Inverse[F[A]]` from an `Inverse[A]`.
   */
  type DeriveInverse[F[_]] = Derive[F, Inverse]

  /**
   * A `Derive` for `Ord`: derives an `Ord[F[A]]` from an `Ord[A]`.
   */
  type DeriveOrd[F[_]] = Derive[F, Ord]

  /**
   * Aliases that give ZIO Prelude's abstractions, or combinations of them, the names used by the traditional functional
   * programming type class hierarchy (`Semigroup`, `Monoid`, `Functor`, `Monad` and so on). Import
   * `zio.prelude.classic._` to use these names.
   *
   * {{{
   * import zio.prelude._
   * import zio.prelude.classic._
   *
   * def double[F[+_]: Functor](fa: F[Int]): F[Int] =
   *   fa.map(_ * 2)
   *
   * val doubled: List[Int] = double(List(1, 2))
   * // List(2, 4)
   * }}}
   */
  object classic {

    /**
     * A `Semigroup` is an `Associative` operation.
     */
    type Semigroup[A] = Associative[A]

    /**
     * A `CommutativeSemigroup` is a `Commutative` operation.
     */
    type CommutativeSemigroup[A] = Commutative[A]

    /**
     * A `Monoid` is an `Identity`: an associative operation with an identity element.
     */
    type Monoid[A] = Identity[A]

    /**
     * A `CommutativeMonoid` is an operation that is both `Commutative` and an `Identity`.
     */
    type CommutativeMonoid[A] = Commutative[A] with Identity[A]

    /**
     * A `Group` is an `Inverse`: an `Identity` with an inverse operation.
     */
    type Group[A] = Inverse[A]

    /**
     * An `AbelianGroup` is an operation that is both `Commutative` and an `Inverse`.
     */
    type AbelianGroup[A] = Commutative[A] with Inverse[A]

    /**
     * A `Semilattice` is an operation that is both `Commutative` and `Idempotent`.
     */
    type Semilattice[A] = Commutative[A] with Idempotent[A]

    /**
     * A `BoundedSemilattice` is a `Semilattice` that is also an `Identity`.
     */
    type BoundedSemilattice[A] = Semilattice[A] with Identity[A]

    /**
     * A `Functor` is a `Covariant` type constructor.
     */
    type Functor[F[+_]] = Covariant[F]

    /**
     * The same type as `zio.prelude.Contravariant`, under the same name.
     */
    type Contravariant[F[-_]] = zio.prelude.Contravariant[F]

    /**
     * The same type as `zio.prelude.Invariant`, under the same name.
     */
    type Invariant[F[_]] = zio.prelude.Invariant[F]

    /**
     * An `Alternative` is a type constructor that is `Covariant`, `IdentityBoth` and `IdentityEither`.
     */
    type Alternative[F[+_]] =
      Covariant[F] with IdentityBoth[F] with IdentityEither[F]

    /**
     * An `InvariantAlt` is a type constructor that is `Invariant`, `IdentityBoth` and `IdentityEither`.
     */
    type InvariantAlt[F[_]] =
      Invariant[F] with IdentityBoth[F] with IdentityEither[F]

    /**
     * An `InvariantSemigroupal` is a type constructor that is `Invariant` and `AssociativeBoth`.
     */
    type InvariantSemigroupal[F[_]] = Invariant[F] with AssociativeBoth[F]

    /**
     * A `Semigroupal` is a type constructor that is `Covariant` and `AssociativeBoth`.
     */
    type Semigroupal[F[+_]] = Covariant[F] with AssociativeBoth[F]

    /**
     * A `ContravariantSemigroupal` is a type constructor that is `Contravariant` and `AssociativeBoth`.
     */
    type ContravariantSemigroupal[F[-_]] = Contravariant[F] with AssociativeBoth[F]

    /**
     * A `SemigroupK` is an `AssociativeEither` type constructor.
     */
    type SemigroupK[F[_]] = AssociativeEither[F]

    /**
     * A `MonoidK` is an `IdentityEither` type constructor.
     */
    type MonoidK[F[_]] = IdentityEither[F]

    /**
     * A `ContravariantMonoidal` is a type constructor that is `Contravariant` and `IdentityBoth`.
     */
    type ContravariantMonoidal[F[-_]] = Contravariant[F] with IdentityBoth[F]

    /**
     * An `InvariantMonoidal` is a type constructor that is `Invariant` and `IdentityBoth`.
     */
    type InvariantMonoidal[F[_]] = Invariant[F] with IdentityBoth[F]

    /**
     * A `FlatMap` is a type constructor that is `Covariant` and `AssociativeFlatten`.
     */
    type FlatMap[F[+_]] = Covariant[F] with AssociativeFlatten[F]

    /**
     * A `Monad` is a type constructor that is `Covariant` and `IdentityFlatten`.
     */
    type Monad[F[+_]] = Covariant[F] with IdentityFlatten[F]

    /**
     * A `Divide` is a type constructor that is `Contravariant` and `AssociativeBoth`.
     */
    type Divide[F[-_]] = Contravariant[F] with AssociativeBoth[F]

    /**
     * A `Divisible` is a type constructor that is `Contravariant` and `IdentityBoth`.
     */
    type Divisible[F[-_]] = Contravariant[F] with IdentityBoth[F]

    /**
     * A `Decidable` is a type constructor that is `Contravariant`, `IdentityBoth` and `IdentityEither`.
     */
    type Decidable[F[-_]] =
      Contravariant[F] with IdentityBoth[F] with IdentityEither[F]

    /**
     * An `Apply` is a type constructor that is `Covariant` and `AssociativeBoth`.
     */
    type Apply[F[+_]] = Covariant[F] with AssociativeBoth[F]

    /**
     * An `Applicative` is a type constructor that is `Covariant` and `IdentityBoth`.
     */
    type Applicative[F[+_]] = Covariant[F] with IdentityBoth[F]

    /**
     * An `InvariantApplicative` is a type constructor that is `Invariant` and `IdentityBoth`.
     */
    type InvariantApplicative[F[_]] = Invariant[F] with IdentityBoth[F]

    /**
     * A `Category` is an `IdentityCompose`: composition of arrows `=>:` with an identity arrow.
     */
    type Category[=>:[-_, +_]] = IdentityCompose[=>:]

    /**
     * A `Profunctor` is a `Divariant` type constructor: contravariant in its first and covariant in its second type
     * parameter.
     */
    type Profunctor[=>:[-_, +_]] = Divariant[=>:]

    /**
     * A `Bifunctor` is a `Bicovariant` type constructor: covariant in both type parameters.
     */
    type Bifunctor[=>:[+_, +_]] = Bicovariant[=>:]
  }
}
