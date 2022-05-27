/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

import zio.NonEmptyChunk
import zio.test.Assertion.Render._
import zio.test.{Assertion => TestAssertion}

/**
 * Provides versions of assertions from _ZIO Test_ that use `Equal`, `Ord`, and
 * `Validation`.
 */
trait TestAssertions {

  /**
   * Makes a new assertion that requires a value equal the specified value.
   */
  def equalTo[A: Equal](expected: A): TestAssertion[A] =
    TestAssertion.assertion("equalTo")(param(expected))(_ === expected)

  /**
   * Makes a new assertion that requires a validation failure satisfying a
   * specified assertion.
   */
  def isFailureV[E](assertion: TestAssertion[NonEmptyChunk[E]]): TestAssertion[ZValidation[Any, E, Any]] =
    TestAssertion.assertionRec("isFailureV")(param(assertion))(assertion) {
      case ZValidation.Failure(_, es) => Some(es)
      case _                          => None
    }

  /**
   * Makes a new assertion that requires the value be greater than the
   * specified reference value.
   */
  def isGreaterThan[A](reference: A)(implicit ord: PartialOrd[A]): TestAssertion[A] =
    TestAssertion.assertion("isGreaterThan")(param(reference))(_ > reference)

  /**
   * Makes a new assertion that requires the value be greater than or equal to
   * the specified reference value.
   */
  def isGreaterThanEqualTo[A](reference: A)(implicit ord: PartialOrd[A]): TestAssertion[A] =
    TestAssertion.assertion("isGreaterThanEqualTo")(param(reference))(_ >= reference)

  /**
   * Makes a new assertion that requires the value be less than the specified
   * reference value.
   */
  def isLessThan[A](reference: A)(implicit ord: PartialOrd[A]): TestAssertion[A] =
    TestAssertion.assertion("isLessThan")(param(reference))(_ < reference)

  /**
   * Makes a new assertion that requires the value be less than or equal to the
   * specified reference value.
   */
  def isLessThanEqualTo[A](reference: A)(implicit ord: PartialOrd[A]): TestAssertion[A] =
    TestAssertion.assertion("isLessThanEqualTo")(param(reference))(_ <= reference)

  /**
   * Makes a new assertion that requires a validation failure satisfying a
   * specified assertion.
   */
  def isSuccessV[A](assertion: TestAssertion[A]): TestAssertion[ZValidation[Any, Any, A]] =
    TestAssertion.assertionRec("isSuccessV")(param(assertion))(assertion) {
      case ZValidation.Success(_, a) => Some(a)
      case _                         => None
    }
}
