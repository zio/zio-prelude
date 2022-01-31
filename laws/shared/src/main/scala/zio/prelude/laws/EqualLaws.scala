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
import zio.test.TestResult
import zio.test.laws._

object EqualLaws extends Lawful[Equal] {

  /**
   * For all values `a1`, `a1` is equal to `a1`.
   */
  lazy val reflexiveLaw: Laws.Law1[Equal] =
    new Laws.Law1[Equal]("reflexiveLaw") {
      def apply[A: Equal](a1: A): TestResult =
        a1 <-> a1
    }

  /**
   * For all values `a1` and `a2`, if `a1` is equal to `a2` then `a2` is equal
   * to `a1`.
   */
  lazy val symmetryLaw: Laws.Law2[Equal] =
    new Laws.Law2[Equal]("symmetryLaw") {
      def apply[A: Equal](a1: A, a2: A): TestResult =
        (a1 <-> a2) ==> (a2 <-> a1)
    }

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is equal to `a2` and `a2` is
   * equal `a3`, then `a1` is equal to `a3`.
   */
  lazy val transitivityLaw: Laws.Law3[Equal] =
    new Laws.Law3[Equal]("transitivityLaw") {
      def apply[A: Equal](a1: A, a2: A, a3: A): TestResult =
        ((a1 <-> a2) && (a2 <-> a3)) ==> (a1 <-> a3)
    }

  /**
   * The set of all laws that instances of `Equal` must satisfy.
   */
  lazy val laws: Laws[Equal] =
    reflexiveLaw + symmetryLaw + transitivityLaw
}
