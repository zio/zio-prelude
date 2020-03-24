package zio.prelude

import scala.annotation.implicitNotFound

import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

/**
 * `Equal[A]` provides implicit evidence that two values of type `A` can be
 * compared for equality.
 */
@implicitNotFound("No implicit Equal defined for ${A}.")
trait Equal[-A] { self =>

  /**
   * Returns whether two values of type `A` are equal.
   */
  def equal(l: A, r: A): Boolean

  /**
   * Constructs an `Equal[(A, B)]` given an `Equal[A]` and `Equal[B]` by first
   * comparing the `A` values for equality and then comparing the `B` values
   * for equality, if necessary.
   */
  final def both[B](that: Equal[B]): Equal[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Equal[C]` given an `Equal[A]`, an `Equal[B]` and a
   * function `f` to transform a `C` value into an `(A, B)`. The instance
   * will convert each `C` value into an `(A, B)`, compare the `A` values for
   * equality, and then compare the `B` values for equality if necessary.
   */
  final def bothWith[B, C](that: Equal[B])(f: C => (A, B)): Equal[C] =
    Equal { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.equal(a1, a2) && that.equal(b1, b2)
      }
    }

  /**
   * Constructs an `Equal[B]` given an `Equal[A]` and a function `f` to
   * transform a `B` value into an `A` value. The instance will convert each
   * `B` value into an `A` and the compare the `A` values for equality.
   */
  def contramap[B](f: B => A): Equal[B] =
    Equal((b1, b2) => self.equal(f(b1), f(b2)))

  /**
   * Constructs an `Equal[Either[A, B]]` given an `Equal[A]` and an
   * `Equal[B]`. The instance will compare the `Either[A, B]` values and if
   * both are `Right` or `Left` compare them for equality.
   */
  final def either[B](that: Equal[B]): Equal[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Equal[C]` given an `Equal[A]`, an `Equal[B]`, and a
   * function `f` to transform a `C` value into an `Either[A, B]`. The
   * instance will convert each `C` value into an `Either[A, B]` and then
   * if both are `Right` or `Left` compare them for equality.
   */
  final def eitherWith[B, C](that: Equal[B])(f: C => Either[A, B]): Equal[C] =
    Equal { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.equal(a1, a2)
        case (Right(b1), Right(b2)) => that.equal(b1, b2)
        case _                      => false
      }
    }

  /**
   * Returns whether two values of type `A` are not equal.
   */
  final def notEqual(l: A, r: A): Boolean = !equal(l, r)
}

object Equal extends Lawful[Equal] {
  final val reflexiveLaw = new Laws.Law1[Equal]("reflexiveLaw") {
    def apply[A: Equal](a1: A): TestResult =
      a1 <-> a1
  }

  final val symmetryLaw = new Laws.Law2[Equal]("symmetryLaw") {
    def apply[A: Equal](a1: A, a2: A): TestResult =
      (a1 <-> a2) ==> (a2 <-> a1)
  }

  final val transitivityLaw = new Laws.Law3[Equal]("transitivityLaw") {
    def apply[A: Equal](a1: A, a2: A, a3: A): TestResult =
      ((a1 <-> a2) && (a2 <-> a3)) ==> (a1 <-> a3)
  }

  final val laws = reflexiveLaw + symmetryLaw + transitivityLaw

  /**
   * Summons an implicit `Equal[A]`.
   */
  def apply[A](implicit equal: Equal[A]): Equal[A] = equal

  /**
   * Constructs an `Equal[A]` from a function. The instance will be optimized
   * to first compare the values for reference equality and then compare the
   * values for value equality.
   */
  def fromFunction[A](eq0: (A, A) => Boolean): Equal[A] = (l: A, r: A) => refEq(l, r) || eq0(l, r)

  /**
   * Constructs an `Equal[A]` that uses the default notion of equality
   * embodied in the implementation of `equals` for values of type `A`.
   */
  def default[A]: Equal[A] = Equal((l: A, r: A) => l == r)

  /**
   * Returns whether two values refer to the same location in memory.
   */
  private[prelude] def refEq[A](l: A, r: A): Boolean =
    l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]
}

trait EqualSyntax {

  /**
   * Provides infix syntax for comparing two values for equality.
   */
  implicit class EqualSyntax[A](l: A) {

    /**
     * Returns whether this value and the specified value are equal.
     */
    def ===(r: A)(implicit equal: Equal[A]): Boolean = equal.equal(l, r)

    /**
     * Returns whether this value and the specified value are not equal.
     */
    def !==(r: A)(implicit equal: Equal[A]): Boolean = equal.notEqual(l, r)
  }
}
