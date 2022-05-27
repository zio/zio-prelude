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
import zio.prelude.coherent.DeriveEqualIdentityBothInvariant
import zio.test.TestResult
import zio.test.laws._

object IdentityBothLaws extends LawfulF.Invariant[DeriveEqualIdentityBothInvariant, Equal] {

  /**
   * For all `fa`, `both(identity, fa)` is equivalent to `fa`.
   */
  lazy val leftIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("leftIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(IdentityBoth[F].any, fa)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A] compose Equivalence.tupleFlip).to(left)
        left2 <-> right
      }
    }

  /**
   * For all `fa`, `both(fa, identity)` is equivalent to `fa`.
   */
  lazy val rightIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("rightIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(fa, IdentityBoth[F].any)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `IdentityBoth` must satisfy.
   */
  lazy val laws: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    leftIdentityLaw + rightIdentityLaw + AssociativeBothLaws.laws
}
