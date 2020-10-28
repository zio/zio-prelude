package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait DivideShape[A, +Addition[x] <: Associative[x], +Multiplication[x] <: InverseNonZero[x]]
    extends AddMultiplyShape[A, Addition, Multiplication] {

  def divide(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.inverse(Prod(l), Prod(r)))
}

object DivideShape {

  def fromMultiplicativeInverse[A, Addition[x] <: Associative[x], Multiplication[x] <: InverseNonZero[x]](implicit
    ev: AddMultiplyShape[A, Addition, Multiplication]
  ): DivideShape[A, Addition, Multiplication] = new DivideShape[A, Addition, Multiplication] {

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: Addition[Sum[A]] = ev.Addition

    override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
  }
}

trait DivideShapeSyntax {

  /**
   * Provides infix syntax for dividing two values.
   */
  implicit class DivideShapeOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def -/-(r: => A)(implicit divide: DivideShape[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

    /**
     * Subtract two values.
     */
    def divide(r: => A)(implicit divide: DivideShape[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

  }

}
