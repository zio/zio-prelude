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

package zio.prelude

/**
 * Experimental abstractions that are not part of the core ZIO Prelude API:
 * lattices and Boolean algebras (`Absorption`, `DistributiveAbsorption`,
 * `Complement`, `ExcludedMiddle`, `Noncontradiction`, `Involution`),
 * arithmetic (`DistributiveProd`, `Annihilation`, `Subtract`,
 * `PartialDivide`, `Divide`) and arrow composition (`BothCompose`,
 * `EitherCompose`, `ApplicationCompose`). Their names and shapes can change
 * between releases.
 *
 * Importing `zio.prelude.experimental._` brings the infix syntax for these
 * type classes into scope. The laws objects, such as `AbsorptionLaws` and
 * `DistributiveProdLaws`, are in the `zio.prelude.experimental.laws` package
 * of zio-prelude-experimental-laws. The composition type classes state their
 * laws as methods instead (`bothCompose`, `eitherCompose`,
 * `applicationCompose`).
 */
package object experimental
    extends AbsorptionSyntax
    with ApplicationComposeSyntax
    with BothComposeSyntax
    with ComplementSyntax
    with DistributiveProdSyntax
    with DivideSyntax
    with EitherComposeSyntax
    with PartialDivideSyntax
    with SubtractSyntax {

  /**
   * Type aliases that combine the composition abstractions with
   * `zio.prelude.classic.Category` under their category-theory names.
   */
  object classic {

    import zio.prelude.classic._

    /**
     * A `Category` with products: `Category[=>:]` with a
     * `BothCompose.Aux[=>:, :*:]`.
     */
    type CartesianCategory[=>:[-_, +_], :*:[+_, +_]] = Category[=>:] with BothCompose.Aux[=>:, :*:]

    /**
     * A `CartesianCategory` with exponentials: it adds an
     * `ApplicationCompose.Aux[=>:, :*:, -->:]`.
     */
    type ClosedCartesianCategory[=>:[-_, +_], :*:[+_, +_], -->:[-_, +_]] = CartesianCategory[=>:, :*:]
      with ApplicationCompose.Aux[=>:, :*:, -->:]

    /**
     * A `Category` with sums: `Category[=>:]` with an
     * `EitherCompose.Aux[=>:, :+:]`.
     */
    type CoCartesianCategory[=>:[-_, +_], :+:[+_, +_]] = Category[=>:] with EitherCompose.Aux[=>:, :+:]
  }

}
