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
import zio.prelude.coherent.CommutativeEqual
import zio.test.TestResult
import zio.test.laws._

object CommutativeLaws extends Lawful[CommutativeEqual] {

  /**
   * The commutative law states that for some binary operator `*`, for all
   * values `a1` and `a2`, the following must hold:
   *
   * {{{
   * a1 * a2 === a2 * a1
   * }}}
   */
  lazy val commutativeLaw: Laws[CommutativeEqual] =
    new Laws.Law2[CommutativeEqual]("commutativeLaw") {
      def apply[A: CommutativeEqual](a1: A, a2: A): TestResult =
        (a1 <> a2) <-> (a2 <> a1)
    }

  /**
   * The set of all laws that instances of `Commutative` must satisfy.
   */
  lazy val laws: Laws[CommutativeEqual] =
    commutativeLaw + AssociativeLaws.laws
}
