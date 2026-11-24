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

package zio.prelude

import zio.test.{TestResult, assert}

/**
 * Provides assertions that compare values with `Equal` and `PartialOrd`, for
 * writing the laws of _ZIO Prelude_ type classes.
 */
package object laws extends TestAssertions {

  /**
   * Provides implicit syntax for assertions.
   */
  implicit class AssertionSyntax[A](private val self: A) extends AnyVal {

    /**
     * Same as `isEqualTo`: a `TestResult` that succeeds when the value equals
     * `that`, compared with `Equal`.
     */
    def <->[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult       =
      isEqualTo(that)
    // name intentionally different from other methods (`equal`, `equalTo`, etc to avoid confusing compiler errors)
    /**
     * A `TestResult` that succeeds when the value equals `that`, compared with
     * `Equal`.
     */
    def isEqualTo[A1 >: A](that: A1)(implicit eq: Equal[A1]): TestResult =
      assert(self)(equalTo(that))

    /**
     * A `TestResult` that succeeds when the value is greater than `that`,
     * compared with `PartialOrd`.
     */
    def greater(that: A)(implicit ord: PartialOrd[A]): TestResult =
      assert(self)(isGreaterThan(that))

    /**
     * A `TestResult` that succeeds when the value is greater than or equal to
     * `that`, compared with `PartialOrd`.
     */
    def greaterOrEqual(that: A)(implicit ord: PartialOrd[A]): TestResult =
      assert(self)(isGreaterThanEqualTo(that))

    /**
     * A `TestResult` that succeeds when the value is less than `that`, compared
     * with `PartialOrd`.
     */
    def less(that: A)(implicit ord: PartialOrd[A]): TestResult =
      assert(self)(isLessThan(that))

    /**
     * A `TestResult` that succeeds when the value is less than or equal to
     * `that`, compared with `PartialOrd`.
     */
    def lessOrEqual(that: A)(implicit ord: PartialOrd[A]): TestResult =
      assert(self)(isLessThanEqualTo(that))
  }
}
