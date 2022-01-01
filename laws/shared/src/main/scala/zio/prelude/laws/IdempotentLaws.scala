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
import zio.prelude.coherent.EqualIdempotent
import zio.test.TestResult
import zio.test.laws._

object IdempotentLaws extends Lawful[EqualIdempotent] {

  /**
   * The idempotent law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * a === a
   * }}}
   */
  lazy val idempotentLaw: Laws[EqualIdempotent] =
    new Laws.Law1[EqualIdempotent]("idempotentLaw") {
      def apply[A: EqualIdempotent](a: A): TestResult =
        (a <> a) <-> a
    }

  /**
   * The set of all laws that instances of `Idempotent` must satisfy.
   */
  lazy val laws: Laws[EqualIdempotent] =
    idempotentLaw + AssociativeLaws.laws
}
