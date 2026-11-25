package zio.prelude

package object newtypes {

  /**
   * Converts values to and from the `Sum` newtype, which marks values that
   * combine by addition.
   */
  object Sum extends SubtypeF

  /**
   * A newtype representing addition.
   */
  type Sum[A] = Sum.Type[A]

  /**
   * Converts values to and from the `Prod` newtype, which marks values that
   * combine by multiplication.
   */
  object Prod extends SubtypeF

  /**
   * A newtype representing multiplication.
   */
  type Prod[A] = Prod.Type[A]

  /**
   * Converts values to and from the `Or` newtype, which marks a `Boolean`
   * that combines by logical disjunction.
   */
  object Or extends Subtype[Boolean] {
    private[prelude] def create(value: Boolean): Or = wrap(value)
  }

  /**
   * A newtype representing logical disjunction.
   */
  type Or = Or.Type

  /**
   * Converts values to and from the `And` newtype, which marks a `Boolean`
   * that combines by logical conjunction.
   */
  object And extends Subtype[Boolean] {
    private[prelude] def create(value: Boolean): And = wrap(value)
  }

  /**
   * A newtype representing logical conjunction.
   */
  type And = And.Type

  /**
   * Converts values to and from the `AndF` newtype, which marks values that
   * combine by a parameterized logical conjunction.
   */
  object AndF extends SubtypeF

  /**
   * A newtype representing parameterized logical conjunction.
   */
  type AndF[+A] = AndF.Type[A]

  /**
   * Converts values to and from the `AndThen` newtype, which marks values
   * that combine one after the other.
   */
  object AndThen extends SubtypeF

  /**
   * A newtype representing sequential composition, such as combining two
   * `Cause` values with `++`.
   */
  type AndThen[+A] = AndThen.Type[A]

  /**
   * Converts values to and from the `Both` newtype, which marks values that
   * combine in parallel.
   */
  object Both extends SubtypeF

  /**
   * A newtype representing parallel composition, such as combining two
   * `Cause` values with `&&`.
   */
  type Both[+A] = Both.Type[A]

  /**
   * Converts values to and from the `OrF` newtype, which marks values that
   * combine by a parameterized logical disjunction.
   */
  object OrF extends SubtypeF

  /**
   * A newtype representing parameterized logical disjunction.
   */
  type OrF[+A] = OrF.Type[A]

  /**
   * Converts values to and from the `First` newtype, which keeps the first
   * of two values when they are combined.
   */
  object First extends SubtypeF

  /**
   * A newtype representing taking the first of two elements.
   */
  type First[A] = First.Type[A]

  /**
   * Converts values to and from the `Last` newtype, which keeps the last of
   * two values when they are combined.
   */
  object Last extends SubtypeF

  /**
   * A newtype representing taking the last of two elements.
   */
  type Last[A] = Last.Type[A]

  /**
   * Converts values to and from the `Min` newtype, which keeps the smaller
   * of two values when they are combined.
   */
  object Min extends SubtypeF

  /**
   * A newtype representing taking the min of two elements.
   */
  type Min[A] = Min.Type[A]

  /**
   * Converts values to and from the `Max` newtype, which keeps the larger of
   * two values when they are combined.
   */
  object Max extends SubtypeF {}

  /**
   * A newtype representing taking the max of two elements.
   */
  type Max[A] = Max.Type[A]

  /**
   * A newtype representing another type in a failed state
   */
  object Failure extends NewtypeF

  /**
   * A newtype representing another type in a failed state. Instances for
   * `Failure` act on the error type, for example of an `Either` or a `ZIO`.
   */
  type Failure[+A] = Failure.Type[A]

  /**
   * A newtype representing an input error in another type
   */
  object FailureIn extends NewtypeF

  /**
   * A newtype representing an input error in another type. The library
   * defines no instances for it.
   */
  type FailureIn[+A] = FailureIn.Type[A]

  /**
   * A newtype representing an output error in another type
   */
  object FailureOut extends NewtypeF

  /**
   * A newtype representing an output error in another type. The library
   * defines no instances for it.
   */
  type FailureOut[+A] = FailureOut.Type[A]

  /**
   * A smart subtype of `Int` whose values are greater than or equal to 0.
   * `Natural(5)` checks a literal at compile time, and `Natural.make` checks
   * a value at run time.
   */
  object Natural extends Subtype[Int] {
    // scalafix:off
    override def assertion = assert(Assertion.greaterThanOrEqualTo(0))
    // scalafix:on

    /**
     * The natural number 0.
     */
    val zero: Natural =
      Natural(0)

    /**
     * The natural number 1.
     */
    val one: Natural =
      Natural(1)

    /**
     * Returns `n + 1`. For `Int.MaxValue` the addition overflows, and the
     * result is `Int.MinValue`, which is negative.
     */
    def successor(n: Natural): Natural =
      wrap(n + 1)

    /**
     * Returns `x * y`, or `Int.MaxValue` when the product overflows `Int`.
     * When `x` is 0 the result is also `Int.MaxValue`; when only `y` is 0 it
     * is 0.
     */
    def times(x: Natural, y: Natural): Natural = {
      val product = x * y
      if (x == 0 || product / x != y) wrap(Int.MaxValue) else wrap(product)
    }

    /**
     * Returns `x + y`, or `Int.MaxValue` when the sum overflows `Int`.
     */
    def plus(x: Natural, y: Natural): Natural = {
      val sum = x + y
      if (sum < 0) wrap(Int.MaxValue) else wrap(sum)
    }

    /**
     * Returns `x - y`, or 0 when `y` is greater than `x`.
     */
    def minus(x: Natural, y: Natural): Natural = {
      val difference = x - y
      if (difference < 0) zero else wrap(difference)
    }

    private[prelude] def unsafeMake(n: Int): Natural =
      wrap(n)
  }

  /**
   * A natural number: an `Int` greater than or equal to 0.
   */
  type Natural = Natural.Type
}
