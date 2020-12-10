package zio.prelude
package experimental

import zio.prelude.newtypes.{ Prod, Sum }

trait SubtractShape[A] extends AddMultiplyShape[A] {

  override type Addition[x] <: Inverse[x]

  def subtract(l: => A, r: => A): A =
    Addition.inverse(Sum(l), Sum(r))
}

object SubtractShape {

  type Aux[A, +addition[x] <: Inverse[x], +multiplication[x] <: Associative[x]] = SubtractShape[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  def fromAdditiveInverse[A, addition[x] <: Inverse[x], multiplication[x] <: Associative[x]](implicit
    ev: AddMultiplyShape.Aux[A, addition, multiplication]
  ): SubtractShape.Aux[A, addition, multiplication] = new SubtractShape[A] {

    override type Addition[x] = addition[x]

    override type Multiplication[x] = multiplication[x]

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: addition[Sum[A]] = ev.Addition

    override def Multiplication: multiplication[Prod[A]] = ev.Multiplication
  }

  def fromAdditiveInverseAndDistributiveMultiply[A, addition[x] <: Inverse[x], multiplication[x] <: Associative[x]](
    implicit ev: DistributiveMultiply.Aux[A, addition, multiplication]
  ): Annihilation.Aux[A, addition, multiplication]
    with DistributiveMultiply.Aux[A, addition, multiplication]
    with SubtractShape.Aux[A, addition, multiplication] =
    new Annihilation[A] with DistributiveMultiply[A] with SubtractShape[A] {

      override type Addition[x] = addition[x]

      override type Multiplication[x] = multiplication[x]

      override def add(l: => A, r: => A): A = ev.add(l, r)

      override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

      override def Addition: addition[Sum[A]] = ev.Addition

      override def Multiplication: multiplication[Prod[A]] = ev.Multiplication
    }
}

trait SubtractShapeSyntax {

  /**
   * Provides infix syntax for subtracting two values.
   */
  implicit class SubtractShapeOps[A](private val l: A) {

    /**
     * A symbolic alias for `subtract`.
     */
    def ---(r: => A)(implicit subtract: SubtractShape.Aux[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

    /**
     * Subtract two values.
     */
    def subtract(r: => A)(implicit subtract: SubtractShape.Aux[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

  }

}
