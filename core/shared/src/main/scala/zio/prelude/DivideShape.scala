package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait DivideShape[A] extends AddMultiplyShape[A] {

  override type Multiplication[x] <: InverseNonZero[x]

  def divide(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.inverse(Prod(l), Prod(r)))
}

object DivideShape {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: InverseNonZero[x]] = DivideShape[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  def fromMultiplicativeInverse[A, addition[x] <: Associative[x], multiplication[x] <: InverseNonZero[x]](implicit
    ev: AddMultiplyShape.Aux[A, addition, multiplication]
  ): DivideShape.Aux[A, addition, multiplication] = new DivideShape[A] {

    override type Addition[x] = addition[x]

    override type Multiplication[x] = multiplication[x]

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: addition[Sum[A]] = ev.Addition

    override def Multiplication: multiplication[Prod[A]] = ev.Multiplication
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
    def -:-(r: => A)(implicit divide: DivideShape.Aux[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

    /**
     * Subtract two values.
     */
    def divide(r: => A)(implicit divide: DivideShape.Aux[A, Associative, InverseNonZero]): A =
      divide.divide(l, r)

  }

}
