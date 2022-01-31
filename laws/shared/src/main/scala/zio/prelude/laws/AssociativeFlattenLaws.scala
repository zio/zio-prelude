/*
 * Copyright 2021-2022 John A. De Goes and the ZIO Contributors
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

package zio.prelude.laws

import zio.prelude._
import zio.prelude.coherent.AssociativeFlattenCovariantDeriveEqual
import zio.test.TestResult
import zio.test.laws._

object AssociativeFlattenLaws extends LawfulF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] {

  /**
   * For all `fffa`, `flatten(flatten(fffa))` is equivalent to
   * `flatten(fffa.map(flatten))`.
   */
  lazy val associativityLaw: LawsF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] =
    new LawsF.Covariant.FlattenLaw[AssociativeFlattenCovariantDeriveEqual, Equal]("associativityLaw") {
      def apply[F[+_]: AssociativeFlattenCovariantDeriveEqual, A: Equal](fffa: F[F[F[A]]]): TestResult =
        fffa.flatten.flatten <-> fffa.map(_.flatten).flatten
    }

  /**
   * The set of all laws that instances of `AssociativeFlatten` must satisfy.
   */
  lazy val laws: LawsF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] =
    associativityLaw
}
