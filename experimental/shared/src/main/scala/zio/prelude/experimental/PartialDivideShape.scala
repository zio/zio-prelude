package zio.prelude
package experimental

import zio.prelude.newtypes.{ Prod, Sum }

trait PartialDivideShape[A] extends AddMultiplyShape[A] {

  override type Multiplication[x] <: PartialInverse[x]

  def divideOption(l: => A, r: => A): Option[A] =
    Multiplication.inverseOption(Prod(l), Prod(r))
}

object PartialDivideShape {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: PartialInverse[x]] = PartialDivideShape[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  def fromMultiplicativeInverse[A, addition[x] <: Associative[x], multiplication[x] <: PartialInverse[x]](implicit
    ev: AddMultiplyShape.Aux[A, addition, multiplication]
  ): PartialDivideShape.Aux[A, addition, multiplication] = new PartialDivideShape[A] {

    override type Addition[x] = addition[x]

    override type Multiplication[x] = multiplication[x]

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: addition[Sum[A]] = ev.Addition

    override def Multiplication: multiplication[Prod[A]] = ev.Multiplication
  }
}

trait PartialDivideShapeSyntax {

  /**
   * Provides infix syntax for dividing two values.
   */
  implicit class PartialDivideShapeOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def -/-(r: => A)(implicit partialDivide: PartialDivideShape.Aux[A, Associative, PartialInverse]): Option[A] =
      partialDivide.divideOption(l, r)

    /**
     * Subtract two values.
     */
    def divideOption(r: => A)(implicit
      partialDivide: PartialDivideShape.Aux[A, Associative, PartialInverse]
    ): Option[A] =
      partialDivide.divideOption(l, r)

  }

}
