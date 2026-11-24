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

/**
 * Laws that instances of `Equivalence` must satisfy. They state that converting
 * a value to the other type and back gives the same value, in both directions.
 */
object EquivalenceLaws extends Lawful2[Equivalence, Equal, Equal] {

  /**
   * For all `a`, converting `a` with `to` and then back with `from` gives `a`.
   */
  lazy val leftIdentity: Laws2[Equivalence, Equal, AnyF] =
    new Laws2.Law1Left[Equivalence, Equal, AnyF]("leftIdentity") {
      def apply[A: Equal, B: AnyF](a: A)(implicit equivalence: Equivalence[A, B]): TestResult =
        equivalence.from(equivalence.to(a)) <-> a
    }

  /**
   * For all `b`, converting `b` with `from` and then back with `to` gives `b`.
   */
  lazy val rightIdentity: Laws2[Equivalence, AnyF, Equal] =
    new Laws2.Law1Right[Equivalence, AnyF, Equal]("rightIdentity") {
      def apply[A: AnyF, B: Equal](b: B)(implicit equivalence: Equivalence[A, B]): TestResult =
        equivalence.to(equivalence.from(b)) <-> b
    }

  /**
   * The set of all laws that instances of `Equivalence` must satisfy.
   */
  lazy val laws: Laws2[Equivalence, Equal, Equal] =
    leftIdentity + rightIdentity
}
