package zio.prelude

import zio.test.Assertion
import zio.test.Assertion.Render._

/**
 * Provides versions of assertions from _ZIO Test_ that use `Equal`, `Ord`, and
 * `Validation`.
 */
trait Assertions {

  /**
   * Makes a new assertion that requires a value equal the specified value.
   */
  def equalTo[A: Equal](expected: A): Assertion[A] =
    Assertion.assertion("equalTo")(param(expected))(_ === expected)

  /**
   * Makes a new assertion that requires a validation failure satisfying a
   * specified assertion.
   */
  def isFailureV[E](assertion: Assertion[NonEmptyMultiSet[E]]): Assertion[Validation[E, Any]] =
    Assertion.assertionRec("isFailureV")(param(assertion))(assertion) {
      case Validation.Failure(es) => Some(es)
      case _                      => None
    }

  /**
   * Makes a new assertion that requires the value be greater than the
   * specified reference value.
   */
  def isGreaterThan[A](reference: A)(implicit ord: Ord[A]): Assertion[A] =
    Assertion.assertion("isGreaterThan")(param(reference))(_ > reference)

  /**
   * Makes a new assertion that requires the value be greater than or equal to
   * the specified reference value.
   */
  def isGreaterThanEqualTo[A](reference: A)(implicit ord: Ord[A]): Assertion[A] =
    Assertion.assertion("isGreaterThanEqualTo")(param(reference))(_ >= reference)

  /**
   * Makes a new assertion that requires the value be less than the specified
   * reference value.
   */
  def isLessThan[A](reference: A)(implicit ord: Ord[A]): Assertion[A] =
    Assertion.assertion("isLessThan")(param(reference))(_ < reference)

  /**
   * Makes a new assertion that requires the value be less than or equal to the
   * specified reference value.
   */
  def isLessThanEqualTo[A](reference: A)(implicit ord: Ord[A]): Assertion[A] =
    Assertion.assertion("isLessThanEqualTo")(param(reference))(_ <= reference)

  /**
   * Makes a new assertion that requires a validation failure satisfying a
   * specified assertion.
   */
  def isSuccessV[A](assertion: Assertion[A]): Assertion[Validation[Any, A]] =
    Assertion.assertionRec("isSuccessV")(param(assertion))(assertion) {
      case Validation.Success(a) => Some(a)
      case _                     => None
    }
}
