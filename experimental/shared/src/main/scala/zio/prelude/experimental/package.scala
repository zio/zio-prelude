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

package zio.prelude

package object experimental
    extends ApplicationComposeSyntax
    with BothComposeSyntax
    with ComplementShapeSyntax
    with EitherComposeSyntax
    with JoinMeetSyntax {

  object classic {

    import zio.prelude.classic._

    type CartesianCategory[=>:[-_, +_], :*:[+_, +_]]                     = Category[=>:] with BothCompose.Aux[=>:, :*:]
    type ClosedCartesianCategory[=>:[-_, +_], :*:[+_, +_], -->:[-_, +_]] = CartesianCategory[=>:, :*:]
      with ApplicationCompose.Aux[=>:, :*:, -->:]
    type CoCartesianCategory[=>:[-_, +_], :+:[+_, +_]]                   = Category[=>:] with EitherCompose.Aux[=>:, :+:]

    type Lattice[A]                  = JoinMeetShape.Aux[A, Semilattice, Semilattice]
    type BoundedLattice[A]           = JoinMeetShape.Aux[A, BoundedSemilattice, BoundedSemilattice]
    type OrthoComplementedLattice[A] = ExcludedMiddle[A] with Involution[A] with Noncontradiction[A] {
      type Join[x] = BoundedSemilattice[x]
      type Meet[x] = BoundedSemilattice[x]
    }
    type DistributiveLattice[A]      = DistributiveJoinMeet.Aux[A, Semilattice, Semilattice]
    type BooleanAlgebra[A]           =
      Absorption[A] with DistributiveJoinMeet[A] with ExcludedMiddle[A] with Noncontradiction[A] {
        type Join[x] = BoundedSemilattice[x]
        type Meet[x] = BoundedSemilattice[x]
      }
  }

}
