package zio.prelude
package experimental

import zio.prelude.newtypes.Prod

/**
 * An experimental type class for a [[PartialDivide]] whose multiplication has
 * a total inverse, so that `divide` returns an `A` rather than an `Option`.
 * ZIO Prelude has no built-in `Divide` instance, so it is for user-defined
 * types. It lives in the experimental module, so its name and shape can change
 * between releases.
 *
 * The infix syntax (`-:-`, `divide`) comes from the `zio.prelude.experimental`
 * package object.
 */
trait Divide[A] extends PartialDivide[A] {

  /**
   * The `Inverse` instance for multiplication, over the `Prod` newtype.
   */
  def Prod: Inverse[Prod[A]]

  /**
   * Divides `l` by `r`. By default it uses the inverse of `Prod`.
   */
  def divide(l: => A, r: => A): A = Prod.inverse(newtypes.Prod(l), newtypes.Prod(r))
}

/**
 * Infix syntax for [[Divide]], mixed into the `zio.prelude.experimental`
 * package object.
 */
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
     * Divides this value by `r`, using the `divide` of the `Divide` instance.
     */
    def divide(r: => A)(implicit divide: Divide[A]): A =
      divide.divide(l, r)
  }
}
