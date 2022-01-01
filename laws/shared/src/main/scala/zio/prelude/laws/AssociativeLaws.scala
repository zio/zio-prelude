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
import zio.prelude.coherent.AssociativeEqual
import zio.test.TestResult
import zio.test.laws._

object AssociativeLaws extends Lawful[AssociativeEqual] {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  lazy val associativityLaw: Laws[AssociativeEqual] =
    new Laws.Law3[AssociativeEqual]("associativityLaw") {
      def apply[A: AssociativeEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
    }

  /**
   * The set of all laws that instances of `Associative` must satisfy.
   */
  lazy val laws: Laws[AssociativeEqual] =
    associativityLaw
}
