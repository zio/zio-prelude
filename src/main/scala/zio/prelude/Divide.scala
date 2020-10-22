package zio.prelude

import zio.prelude.newtypes.Prod

trait Divide[A, +Addition[x] <: Associative[x], +Multiplication[x] <: InverseNonZero[x]]
    extends AddMultiply[A, Addition, Multiplication] {

  def divide(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.inverse(Prod(l), Prod(r)))
}

trait DivideSyntax {

  /**
   * Provides infix syntax for dividing two values.
   */
  implicit class DivideOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def -/-(r: => A)(implicit divide: Divide[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

    /**
     * Subtract two values.
     */
    def divide(r: => A)(implicit divide: Divide[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

  }

}
