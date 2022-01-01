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
import zio.prelude.coherent.AssociativeEitherDeriveEqualInvariant
import zio.test.TestResult
import zio.test.laws._

object AssociativeEitherLaws extends LawfulF.Invariant[AssociativeEitherDeriveEqualInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `either(fa, either(fb, fc))` is
   * equivalent to `either(either(fa, fb), fc)`.
   */
  lazy val associativityLaw: LawsF.Invariant[AssociativeEitherDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law3[AssociativeEitherDeriveEqualInvariant, Equal]("associativityLaw") {
      def apply[F[_]: AssociativeEitherDeriveEqualInvariant, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        fb: F[B],
        fc: F[C]
      ): TestResult = {
        val left  = fa.orElseEither(fb.orElseEither(fc))
        val right = (fa.orElseEither(fb)).orElseEither(fc)
        val left2 = Invariant[F].invmap(Equivalence.either[A, B, C]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `AssociativeEither` must
   * satisfy.
   */
  lazy val laws: LawsF.Invariant[AssociativeEitherDeriveEqualInvariant, Equal] =
    associativityLaw
}
