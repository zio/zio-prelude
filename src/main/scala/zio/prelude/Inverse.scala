package zio.prelude

import zio.prelude.coherent.EqualInverse
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Inverse` type class describes an associative binary operator for a
 * type `A` that has an identity element and an inverse binary operator.
 * Combining any value with itself with the inverse operator must return the
 * identity element. For example, for integer addition zero is an identty
 * element and subtraction is an inverse operation, because subtracting any
 * value from itself always returns zero.
 *
 * Because `Inverse` defines a binary rather than a unary operator it can be
 * used to describe inverse operations for types that do not have inverse
 * values. For example, the natural numbers do not have inverses because the
 * set of natural numbers does not include negative numbers. But we can still
 * define a subtraction operation that is the inverse of addition for the
 * natural numbers, since subtracting a number from itself always returns
 * zero.
 */
trait Inverse[A] extends Identity[A] {
  def inverse(l: => A, r: => A): A
}

object Inverse extends Lawful[EqualInverse] {

  /**
   * The inverse law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * a === identity
   * }}}
   */
  val inverseLaw: Laws[EqualInverse] =
    new Laws.Law1[EqualInverse]("rightInverseLaw") {
      def apply[A](a: A)(implicit I: EqualInverse[A]): TestResult =
        I.inverse(a, a) <-> I.identity
    }

  /**
   * The set of all laws that instances of `Inverse` must satisfy.
   */
  val laws: Laws[EqualInverse] =
    inverseLaw + Identity.laws

  /**
   * Summons an implicit `Inverse[A]`.
   */
  def apply[A](implicit Inverse: Inverse[A]): Inverse[A] = Inverse

  /**
   * Constructs an `Inverse` instance from an associative binary operator, an
   * identity element, and an inverse binary operator.
   */
  def make[A](identity0: A, op: (A, A) => A, inv: (A, A) => A): Inverse[A] =
    new Inverse[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
      def inverse(l: => A, r: => A): A = inv(l, r)
    }

  /**
   * Derives an `Inverse[F[A]]` given a `Derive[F, Inverse]` and an
   * `Inverse[A]`.
   */
  implicit def DeriveInverse[F[_], A](implicit derive: Derive[F, Inverse], inverse: Inverse[A]): Inverse[F[A]] =
    derive.derive(inverse)
}
