package zio.prelude
package experimental

import zio.prelude.newtypes.Prod

trait PartialDivide[A] extends DistributiveProd[A] {
  def Prod: PartialInverse[Prod[A]]
  def divideOption(l: => A, r: => A): Option[A] = Prod.inverseOption(newtypes.Prod(l), newtypes.Prod(r))
}

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
     * Divides `l` by `r`, possibly failing.
     */
    def divideOption(r: => A)(implicit partialDivide: PartialDivide[A]): Option[A] =
      partialDivide.divideOption(l, r)
  }
}
