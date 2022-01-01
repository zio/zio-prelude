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
import zio.prelude.coherent.AssociativeBothDeriveEqualInvariant
import zio.test.TestResult
import zio.test.laws._

object AssociativeBothLaws extends LawfulF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `both(fa, both(fb, fc))` is equivalent
   * to `both(both(fa, fb), fc)`.
   */
  lazy val associativityLaw: LawsF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law3[AssociativeBothDeriveEqualInvariant, Equal]("associativityLaw") {
      def apply[F[_]: AssociativeBothDeriveEqualInvariant, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        fb: F[B],
        fc: F[C]
      ): TestResult = {
        val left  = fa.zip(fb.zip(fc))
        val right = (fa.zip(fb)).zip(fc)
        val left2 = Invariant[F].invmap(Equivalence.tuple[A, B, C]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `AssociativeBoth` must satisfy.
   */
  lazy val laws: LawsF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] =
    associativityLaw
}
