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

object PartialOrdLaws extends Lawful[PartialOrd] {

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is less than `a2` and `a2` is
   * less than `a3` then `a1` is less than `a3`.
   */
  lazy val transitivityLaw1: Laws[PartialOrd] =
    new Laws.Law3[PartialOrd]("transitivityLaw1") {
      def apply[A: PartialOrd](a1: A, a2: A, a3: A): TestResult =
        ((a1 less a2) && (a2 less a3)) ==> (a1 less a3)
    }

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is greater than `a2` and `a2`
   * is greater than `a3` then `a1` is greater than `a3`.
   */
  lazy val transitivityLaw2: Laws[PartialOrd] =
    new Laws.Law3[PartialOrd]("transitivityLaw2") {
      def apply[A: PartialOrd](a1: A, a2: A, a3: A): TestResult =
        ((a1 greater a2) && (a2 greater a3)) ==> (a1 greater a3)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is less than or equal to `a2` and
   * `a2` is less than or equal to `a1` then `a1` is equal to `a2`.
   */
  lazy val antisymmetryLaw1: Laws[PartialOrd] =
    new Laws.Law2[PartialOrd]("antisymmetryLaw1") {
      def apply[A: PartialOrd](a1: A, a2: A): TestResult =
        ((a1 lessOrEqual a2) && (a2 lessOrEqual a1)) ==> (a1 isEqualTo a2)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is greater than or equal to `a2` and
   * `a2` is greater than or equal to `a1` then `a1` is equal to `a2`.
   */
  lazy val antisymmetryLaw2: Laws[PartialOrd] =
    new Laws.Law2[PartialOrd]("antisymmetryLaw2") {
      def apply[A: PartialOrd](a1: A, a2: A): TestResult =
        ((a1 greaterOrEqual a2) && (a2 greaterOrEqual a1)) ==> (a1 isEqualTo a2)
    }

  /**
   * For all values `a1` and `a2`, iff `a1 =??= a2` is `Ordering.Equals` then `a1 === a2`.
   */
  lazy val eqConsistencyLaw: Laws[PartialOrd] =
    new Laws.Law2[PartialOrd]("eqConsistencyLaw") {
      def apply[A: PartialOrd](a1: A, a2: A): TestResult =
        ((a1 =??= a2) isEqualTo Ordering.Equals) <==> ((a1 === a2) isEqualTo true)
    }

  /**
   * The set of all laws that instances of `PartialOrd` must satisfy.
   */
  lazy val laws: Laws[PartialOrd] =
    transitivityLaw1 +
      transitivityLaw2 +
      antisymmetryLaw1 +
      antisymmetryLaw2 +
      eqConsistencyLaw +
      EqualLaws.laws
}
