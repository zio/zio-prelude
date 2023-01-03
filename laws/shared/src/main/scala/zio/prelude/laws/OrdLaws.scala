/*
 * Copyright 2021-2023 John A. De Goes and the ZIO Contributors
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
import zio.test.TestResult
import zio.test.laws._

object OrdLaws extends Lawful[Ord] {

  /**
   * For all values `a1` and `a2`, `a1` is less than or equal to `a2` or `a2`
   * is less than or equal to `a1`.
   */
  lazy val connexityLaw1: Laws[Ord] =
    new Laws.Law2[Ord]("connexityLaw1") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 lessOrEqual a2) || (a2 lessOrEqual a1)
    }

  /**
   * For all values `a1` and `a2`, `a1` is greater than or equal to `a2` or
   * `a2` is greater than or equal to `a1`.
   */
  lazy val connexityLaw2: Laws[Ord] =
    new Laws.Law2[Ord]("connexityLaw2") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 greaterOrEqual a2) || (a2 greaterOrEqual a1)
    }

  /**
   * For all values `a1` and `a2`, `a1` is less than or equal to `a2` if and
   * only if `a2` is greater than or equal to `a1`.
   */
  lazy val complementLaw: Laws[Ord] =
    new Laws.Law2[Ord]("complementLaw") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 lessOrEqual a2) <==> (a2 greaterOrEqual a1)
    }

  /**
   * The set of all laws that instances of `Ord` must satisfy.
   */
  lazy val laws: Laws[Ord] =
    connexityLaw1 +
      connexityLaw2 +
      complementLaw +
      PartialOrdLaws.laws
}
