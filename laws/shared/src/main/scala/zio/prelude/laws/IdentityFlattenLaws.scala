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
import zio.prelude.coherent.CovariantDeriveEqualIdentityFlatten
import zio.test.TestResult
import zio.test.laws._

object IdentityFlattenLaws extends LawfulF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] {

  /**
   * Adding a layer by mapping a value and mapping it into the identity
   * element and then flattening is an identity.
   */
  val rightIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("rightIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        fa.map(a => IdentityFlatten[F].any.map(_ => a)).flatten <-> fa
    }

  /**
   * Adding a layer by mapping a value into the identity element and then
   * flattening is an identity
   */
  val leftIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("leftIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        IdentityFlatten[F].any.map(_ => fa).flatten <-> fa
    }

  /**
   * The set of all laws that instances of `IdentityFlatten` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    rightIdentityLaw + leftIdentityLaw + AssociativeFlattenLaws.laws
}
