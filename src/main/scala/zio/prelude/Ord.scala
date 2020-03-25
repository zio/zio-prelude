package zio.prelude

import scala.annotation.implicitNotFound

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Ord[A]` provides implicit evidence that values of type `A` have a total
 * ordering.
 */
@implicitNotFound("No implicit Ord defined for ${A}.")
trait Ord[-A] extends Equal[A] { self =>

  def compare(l: A, r: A): Ordering

  def equal(l: A, r: A): Boolean = compare(l, r).isEqual

  /**
   * Constructs an `Ord[(A, B)]` given an `Ord[A]` and `Ord[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  def both[B](that: Ord[B]): Ord[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  def bothWith[B, C](that: Ord[B])(f: C => (A, B)): Ord[C] =
    Ord { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.compare(a1, a2) <> that.compare(b1, b2)
      }
    }

  /**
   * Constructs an `Ord[B]` given an `Ord[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  override def contramap[B](f: B => A): Ord[B] =
    Ord((b1, b2) => self.compare(f(b1), f(b2)))

  /**
   * Constructs an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  def either[B](that: Ord[B]): Ord[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  def eitherWith[B, C](that: Ord[B])(f: C => Either[A, B]): Ord[C] =
    Ord { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.compare(a1, a2)
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => that.compare(b1, b2)
      }
    }

  /**
   * Constructs a new `Ord[A]` by mapping the result of this ordering using the
   * specified function.
   */
  def mapOrdering(f: Ordering => Ordering): Ord[A] =
    Ord((l, r) => f(self.compare(l, r)))

  /**
   * Returns a new ordering that is the reverse of this one.
   */
  def reverse: Ord[A] =
    mapOrdering(_.opposite)
}

object Ord extends Lawful[Ord] {
  final val transitivityLaw1 = new Laws.Law3[Ord]("transitivityLaw1") {
    def apply[A: Ord](a1: A, a2: A, a3: A): TestResult =
      ((a1 < a2) && (a2 < a3)) ==> (a1 < a3)
  }

  final val transitivityLaw2 = new Laws.Law3[Ord]("transitivityLaw2") {
    def apply[A: Ord](a1: A, a2: A, a3: A): TestResult =
      ((a1 > a2) && (a2 > a3)) ==> (a1 > a3)
  }

  final val antisymmetryLaw1 = new Laws.Law2[Ord]("antisymmetryLaw1") {
    def apply[A: Ord](a1: A, a2: A): TestResult =
      ((a1 <= a2) && (a2 <= a1)) ==> (a1 === a2)
  }

  final val antisymmetryLaw2 = new Laws.Law2[Ord]("antisymmetryLaw2") {
    def apply[A: Ord](a1: A, a2: A): TestResult =
      ((a1 >= a2) && (a2 >= a1)) ==> (a1 === a2)
  }

  final val connexityLaw1 = new Laws.Law2[Ord]("connexityLaw1") {
    def apply[A: Ord](a1: A, a2: A): TestResult =
      (a1 <= a2) or (a2 <= a1)
  }

  final val connexityLaw2 = new Laws.Law2[Ord]("connexityLaw2") {
    def apply[A: Ord](a1: A, a2: A): TestResult =
      (a1 >= a2) or (a2 >= a1)
  }

  final val complementLaw = new Laws.Law2[Ord]("complementLaw") {
    def apply[A: Ord](a1: A, a2: A): TestResult =
      (a1 <= a2) <==> (a2 >= a1)
  }

  final val laws = transitivityLaw1 +
    transitivityLaw2 +
    antisymmetryLaw1 +
    antisymmetryLaw2 +
    connexityLaw1 +
    connexityLaw2 +
    complementLaw

  /**
   * Summons an implicit `Ord[A]`.
   */
  def apply[A](implicit ord: Ord[A]): Ord[A] = ord

  /**
   * Constructs an `Ord[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def fromFunction[A](f: (A, A) => Ordering): Ord[A] =
    new Ord[A] {
      def compare(l: A, r: A): Ordering =
        if (Equal.refEq(l, r)) Ordering.Equals else f(l, r)
    }

  def fromScalaOrdering[A](ordering: scala.math.Ordering[A]): Ord[A] =
    fromFunction((l: A, r: A) =>
      ordering.compare(l, r) match {
        case x if x < 0  => Ordering.LessThan
        case x if x == 0 => Ordering.Equals
        case _           => Ordering.GreaterThan
      }
    )
}

trait OrdSyntax {

  /**
   * Provides infix syntax for comparing two values with a total ordering.
   */
  implicit class OrdSyntax[A](val l: A) {

    /**
     * Returns whether this value is greater than the specified value.
     */
    def >(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.GreaterThan

    /**
     * Returns whether this value is greater than or equal to the specified
     * value.
     */
    def >=(r: A)(implicit ord: Ord[A]): Boolean =
      (l > r) || (l === r)

    /**
     * Returns whether this value is less than the specified value.
     */
    def <(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.LessThan

    /**
     * Returns whether this value is less than or equal to the specified
     * value.
     */
    def <=(r: A)(implicit ord: Ord[A]): Boolean =
      (l < r) || (l === r)

    /**
     * Returns the result of comparing this value with the specified value.
     */
    def =?=(r: A)(implicit ord: Ord[A]): Ordering = ord.compare(l, r)
  }
}

/**
 * An `Ordering` is the result of comparing two values. The result may be
 * `LessThan`, `Equals`, or `GreaterThan`.
 */
sealed trait Ordering { self =>

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>(that: => Ordering): Ordering =
    self orElse that

  /**
   * Returns whether this `Ordering` is `Ordering.Equals`.
   */
  final def isEqual: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Returns whether this `Ordering` is `Ordering.GreaterThan`.
   */
  final def isGreaterThan: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Returns whether this `Ordering` is `Ordering.LessThan`.
   */
  final def isLessThan: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Converts this `Ordering` to an ordinal representation, with `0`
   * representing `LessThan`, `1` representing `Equals` and `2` representing
   * `GreaterThan`.
   */
  final def ordinal: Int = self match {
    case Ordering.LessThan    => 0
    case Ordering.Equals      => 1
    case Ordering.GreaterThan => 2
  }

  /**
   * Returns this ordering, but if this ordering is equal returns the
   * specified ordering.
   */
  final def orElse(that: => Ordering): Ordering = self match {
    case Ordering.Equals => that
    case ordering        => ordering
  }

  /**
   * Returns the opposite of this `Ordering`, with `LessThan` converted to
   * `GreaterThan` and `GreaterThan` converted to `LessThan`.
   */
  final def opposite: Ordering = self match {
    case Ordering.LessThan    => Ordering.GreaterThan
    case Ordering.Equals      => Ordering.Equals
    case Ordering.GreaterThan => Ordering.LessThan
  }
}

object Ordering {
  case object LessThan    extends Ordering
  case object Equals      extends Ordering
  case object GreaterThan extends Ordering

  /**
   * Converts an integer result from [[scala.math.Ordering.compare]] or
   * [[java.lang.Comparable]] to a `Compare`.
   */
  def fromCompare(n: Int): Ordering =
    if (n < 0) LessThan
    else if (n > 0) GreaterThan
    else Equals

  /**
   * Ordering for `Ordering` values.
   */
  implicit val orderingOrdering: Ord[Ordering] =
    Ord((l: Ordering, r: Ordering) => Ord[Int].compare(l.ordinal, r.ordinal))
}
