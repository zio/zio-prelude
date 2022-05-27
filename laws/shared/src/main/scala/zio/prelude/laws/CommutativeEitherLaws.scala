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
import zio.prelude.coherent.CommutativeEitherDeriveEqualInvariant
import zio.test.TestResult
import zio.test.laws._

object CommutativeEitherLaws extends LawfulF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `either(fa, fb)` is equivalent to `either(fb, fa)`.
   */
  lazy val commutativeLaw: LawsF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law2[CommutativeEitherDeriveEqualInvariant, Equal]("commutativeLaw") {
      def apply[F[_]: CommutativeEitherDeriveEqualInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.orElseEitherPar(fb)
        val right = fb.orElseEitherPar(fa)
        val left2 = Invariant[F].invmap(Equivalence.eitherFlip[A, B]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `CommutativeEither` must satisfy.
   */
  lazy val laws: LawsF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] =
    commutativeLaw + AssociativeEitherLaws.laws
}
