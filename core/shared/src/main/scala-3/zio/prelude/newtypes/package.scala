package zio.prelude

import scala.quoted._

package object newtypes {

  object Sum extends SubtypeF

  /**
   * A newtype representing addition.
   */
  type Sum[A] = Sum.Type[A]

  object Prod extends SubtypeF

  /**
   * A newtype representing multiplication.
   */
  type Prod[A] = Prod.Type[A]

  object Or extends Subtype[Boolean] {
    private[prelude] def create(value: Boolean): Or = wrap(value)
  }

  /**
   * A newtype representing logical disjunction.
   */
  type Or = Or.Type

  object And extends Subtype[Boolean] {
    private[prelude] def create(value: Boolean): And = wrap(value)
  }

  /**
   * A newtype representing logical conjunction.
   */
  type And = And.Type

  object AndF extends SubtypeF

  /**
   * A newtype representing parameterized logical conjunction.
   */
  type AndF[+A] = AndF.Type[A]

  object AndThen extends SubtypeF

  type AndThen[+A] = AndThen.Type[A]

  object Both extends SubtypeF

  type Both[+A] = Both.Type[A]

  object OrF extends SubtypeF

  /**
   * A newtype representing parameterized logical disjunction.
   */
  type OrF[+A] = OrF.Type[A]

  object First extends SubtypeF

  /**
   * A newtype representing taking the first of two elements.
   */
  type First[A] = First.Type[A]

  object Last extends SubtypeF

  /**
   * A newtype representing taking the last of two elements.
   */
  type Last[A] = Last.Type[A]

  object Min extends SubtypeF

  /**
   * A newtype representing taking the min of two elements.
   */
  type Min[A] = Min.Type[A]

  object Max extends SubtypeF {}

  /**
   * A newtype representing taking the max of two elements.
   */
  type Max[A] = Max.Type[A]

  /**
   * A newtype representing another type in a failed state
   */
  object Failure extends NewtypeF

  type Failure[+A] = Failure.Type[A]

  /**
   * A newtype representing an input error in another type
   */
  object FailureIn extends NewtypeF

  type FailureIn[+A] = FailureIn.Type[A]

  /**
   * A newtype representing an output error in another type
   */
  object FailureOut extends NewtypeF

  type FailureOut[+A] = FailureOut.Type[A]

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {

    override inline def assertion = Assertion.greaterThanOrEqualTo(0)

    val one: Natural =
      Natural(1)

    val zero: Natural =
      Natural(0)

    val max: Natural =
      Natural(Int.MaxValue)

    def successor(n: Natural): Natural =
      wrap(n + 1)

    def times(x: Natural, y: Natural): Natural = {
      val product = x * y
      if (x == 0 || product / x != y) max else wrap(product)
    }

    def plus(x: Natural, y: Natural): Natural = {
      val sum = x + y
      if (sum < 0) max else wrap(sum)
    }

    def minus(x: Natural, y: Natural): Natural = {
      val difference = x - y
      if (difference < 0) zero else wrap(difference)
    }

    private[prelude] def unsafeMake(n: Int): Natural =
      wrap(n)
  }
}
