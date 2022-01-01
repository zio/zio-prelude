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
import zio.prelude.coherent.CommutativeBothDeriveEqualInvariant
import zio.test.TestResult
import zio.test.laws._

object CommutativeBothLaws extends LawfulF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `both(fa, fb)` is equivalent to `both(fb, fa)`.
   */
  lazy val commutativeLaw: LawsF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law2[CommutativeBothDeriveEqualInvariant, Equal]("commutativeLaw") {
      def apply[F[_]: CommutativeBothDeriveEqualInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.zipPar(fb)
        val right = fb.zipPar(fa)
        val left2 = Invariant[F].invmap(Equivalence.tupleFlip[A, B]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `CommutativeBoth` must satisfy.
   */
  lazy val laws: LawsF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] =
    commutativeLaw + AssociativeBothLaws.laws
}
