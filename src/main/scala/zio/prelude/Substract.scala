package zio.prelude

import zio.prelude.newtypes.Sum

trait Substract[A, +Addition[x] <: Inverse[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiply[A, Addition, Multiplication] {

  def subtract(l: => A, r: => A): A =
    Sum.unwrap(Addition.inverse(Sum(l), Sum(r)))
}

trait SubstractSyntax {

  /**
   * Provides infix syntax for subtracting two values.
   */
  implicit class SubstractOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def ---(r: => A)(implicit substract: Substract[A, Inverse, Associative]): A =
      substract.subtract(l, r)

    /**
     * Subtract two values.
     */
    def subtract(r: => A)(implicit substract: Substract[A, Inverse, Associative]): A =
      substract.subtract(l, r)

  }

}
