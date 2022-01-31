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

import zio.prelude.coherent.EqualInverse
import zio.test.TestResult
import zio.test.laws._

object InverseLaws extends Lawful[EqualInverse] {

  /**
   * The inverse law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * a === identity
   * }}}
   */
  lazy val inverseLaw: Laws[EqualInverse] =
    new Laws.Law1[EqualInverse]("rightInverseLaw") {
      def apply[A](a: A)(implicit I: EqualInverse[A]): TestResult =
        I.inverse(a, a) <-> I.identity
    }

  /**
   * The set of all laws that instances of `Inverse` must satisfy.
   */
  lazy val laws: Laws[EqualInverse] =
    inverseLaw + IdentityLaws.laws
}
