package zio.prelude

import zio.prelude.newtypes.Sum

trait Subtract[A, +Addition[x] <: Inverse[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiply[A, Addition, Multiplication] {

  def subtract(l: => A, r: => A): A =
    Sum.unwrap(Addition.inverse(Sum(l), Sum(r)))
}

trait SubtractSyntax {

  /**
   * Provides infix syntax for subtracting two values.
   */
  implicit class SubtractOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def ---(r: => A)(implicit subtract: Subtract[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

    /**
     * Subtract two values.
     */
    def subtract(r: => A)(implicit subtract: Subtract[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

  }

}
