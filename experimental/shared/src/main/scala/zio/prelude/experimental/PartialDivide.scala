package zio.prelude
package experimental

import zio.prelude.newtypes.Prod

/**
 * An experimental type class for a [[DistributiveProd]] whose multiplication
 * has a partial inverse, which gives a division that can fail, for example
 * when the divisor is zero. It lives in the experimental module, so its name
 * and shape can change between releases.
 *
 * The built-in instances are for `Float`, `Double` and `BigDecimal`, in the
 * `DistributiveProd` companion, and they return `None` when the divisor is
 * zero. The infix syntax (`-/-`, `divideOption`) comes from the
 * `zio.prelude.experimental` package object.
 *
 * {{{
 * val quotient: Option[Double] = 2.5 -/- 0.5
 * // Some(5.0)
 * val byZero: Option[Double]   = 1.0 -/- 0.0
 * // None
 * }}}
 */
trait PartialDivide[A] extends DistributiveProd[A] {

  /**
   * The `PartialInverse` instance for multiplication, over the `Prod`
   * newtype.
   */
  def Prod: PartialInverse[Prod[A]]

  /**
   * Divides `l` by `r`, returning `None` when the division is not defined. By
   * default it uses the partial inverse of `Prod`.
   */
  def divideOption(l: => A, r: => A): Option[A] = Prod.inverseOption(newtypes.Prod(l), newtypes.Prod(r))
}

/**
 * Infix syntax for [[PartialDivide]], mixed into the
 * `zio.prelude.experimental` package object.
 */
trait PartialDivideSyntax {

  /**
   * Provides infix syntax for dividing two values, with possible failure.
   */
  implicit class PartialDivideOps[A](private val l: A) {

    /**
     * A symbolic alias for `divideOption`.
     */
    def -/-(r: => A)(implicit partialDivide: PartialDivide[A]): Option[A] =
      partialDivide.divideOption(l, r)

    /**
     * Divides this value by `r`, returning `None` when the `PartialDivide`
     * instance cannot divide (for the built-in instances, when `r` is zero).
     */
    def divideOption(r: => A)(implicit partialDivide: PartialDivide[A]): Option[A] =
      partialDivide.divideOption(l, r)
  }
}
