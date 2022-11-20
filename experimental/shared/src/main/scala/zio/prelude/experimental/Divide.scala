package zio.prelude
package experimental

import zio.prelude.newtypes.Prod

trait Divide[A] extends PartialDivide[A] {
  def Prod: Inverse[Prod[A]]
  def divide(l: => A, r: => A): A = Prod.inverse(newtypes.Prod(l), newtypes.Prod(r))
}

trait DivideSyntax {

  /**
   * Provides infix syntax for dividing two values.
   */
  implicit class DivideOps[A](private val l: A) {

    /**
     * A symbolic alias for `divide`.
     */
    def -:-(r: => A)(implicit divide: Divide[A]): A =
      divide.divide(l, r)

    /**
     * Divides `l` by `r`.
     */
    def divide(r: => A)(implicit divide: Divide[A]): A =
      divide.divide(l, r)
  }
}
