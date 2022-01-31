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
import zio.prelude.coherent.EqualIdentity
import zio.test.TestResult
import zio.test.laws._

object IdentityLaws extends Lawful[EqualIdentity] {

  /**
   * The left identity law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * identity * a === a
   * }}}
   */
  lazy val leftIdentityLaw: Laws[EqualIdentity] =
    new Laws.Law1[EqualIdentity]("leftIdentityLaw") {
      def apply[A](a: A)(implicit I: EqualIdentity[A]): TestResult =
        (I.identity <> a) <-> a
    }

  /**
   * The left identity law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * identity === a
   * }}}
   */
  lazy val rightIdentityLaw: Laws[EqualIdentity] =
    new Laws.Law1[EqualIdentity]("rightIdentityLaw") {
      def apply[A](a: A)(implicit I: EqualIdentity[A]): TestResult =
        (a <> I.identity) <-> a
    }

  /**
   * The set of all laws that instances of `Identity` must satisfy.
   */
  lazy val laws: Laws[EqualIdentity] =
    leftIdentityLaw + rightIdentityLaw + AssociativeLaws.laws
}
