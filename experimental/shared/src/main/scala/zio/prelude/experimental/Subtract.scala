package zio.prelude
package experimental
import zio.prelude.newtypes.Sum

/**
 * An experimental type class for a [[DistributiveProd]] whose addition has an
 * inverse, which gives subtraction. It has no laws object of its own: the
 * inverse laws belong to the `Inverse` instance of `Sum`. It lives in the
 * experimental module, so its name and shape can change between releases.
 *
 * The built-in instances are for `Byte`, `Char`, `Short`, `Int`, `Long`,
 * `Float`, `Double` and `BigDecimal`, in the `DistributiveProd` companion. The
 * infix syntax (`---`, `subtract`) comes from the `zio.prelude.experimental`
 * package object.
 *
 * {{{
 * val difference: Int = 10 --- 4
 * // 6
 * }}}
 */
trait Subtract[A] extends DistributiveProd[A] {

  /**
   * The `Inverse` instance for addition, over the `Sum` newtype.
   */
  def Sum: Inverse[Sum[A]]

  /**
   * Subtracts `r` from `l`. By default it uses the inverse of `Sum`.
   */
  def subtract(l: => A, r: => A): A = Sum.inverse(newtypes.Sum(l), newtypes.Sum(r))
}

/**
 * Infix syntax for [[Subtract]], mixed into the `zio.prelude.experimental`
 * package object.
 */
trait SubtractSyntax {

  /**
   * Provides infix syntax for subtracting two values.
   */
  implicit class SubtractOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def ---(r: => A)(implicit subtract: Subtract[A]): A =
      subtract.subtract(l, r)

    /**
     * Subtracts `r` from this value, using the `subtract` of the `Subtract`
     * instance.
     */
    def subtract(r: => A)(implicit subtract: Subtract[A]): A =
      subtract.subtract(l, r)
  }
}
