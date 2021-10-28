package zio.prelude
package experimental
import zio.prelude.newtypes.Sum

trait Subtract[A] extends DistributiveProd[A] {
  def Sum: Inverse[Sum[A]]
  def subtract(l: => A, r: => A): A = Sum.inverse(newtypes.Sum(l), newtypes.Sum(r))
}

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
     * Subtract two values.
     */
    def subtract(r: => A)(implicit subtract: Subtract[A]): A =
      subtract.subtract(l, r)
  }
}
