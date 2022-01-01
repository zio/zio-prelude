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
import zio.test.laws.{Lawful, Laws}

object HashLaws extends Lawful[Hash] {

  /**
   * For all values `a1` and `a2`, if `a1` is equal to `a2` then the hash of
   * `a1` is equal to the hash of `a2`.
   */
  lazy val consistencyLaw: Laws[Hash] =
    new Laws.Law2[Hash]("consistencyLaw") {
      def apply[A](a1: A, a2: A)(implicit caps: Hash[A]): TestResult =
        (a1 <-> a2) ==> (Hash[A].hash(a1) <-> Hash[A].hash(a2))
    }

  /**
   * The set of all laws that instances of `Hash` must satisfy.
   */
  lazy val laws: Laws[Hash] =
    consistencyLaw + EqualLaws.laws
}
