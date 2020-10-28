package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait SubtractShape[A, +Addition[x] <: Inverse[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiplyShape[A, Addition, Multiplication] {

  def subtract(l: => A, r: => A): A =
    Sum.unwrap(Addition.inverse(Sum(l), Sum(r)))
}

object SubtractShape {

  def fromAdditiveInverse[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    ev: AddMultiplyShape[A, Addition, Multiplication]
  ): SubtractShape[A, Addition, Multiplication] = new SubtractShape[A, Addition, Multiplication] {

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: Addition[Sum[A]] = ev.Addition

    override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
  }

  def fromAdditiveInverseAndDistributiveMultiply[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](
    implicit ev: DistributiveMultiply[A, Addition, Multiplication]
  ): AnnihilatingZero[A, Addition, Multiplication]
    with DistributiveMultiply[A, Addition, Multiplication]
    with SubtractShape[A, Addition, Multiplication] =
    new AnnihilatingZero[A, Addition, Multiplication]
      with DistributiveMultiply[A, Addition, Multiplication]
      with SubtractShape[A, Addition, Multiplication] {

      override def add(l: => A, r: => A): A = ev.add(l, r)

      override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

      override def Addition: Addition[Sum[A]] = ev.Addition

      override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
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
    def ---(r: => A)(implicit subtract: SubtractShape[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

    /**
     * Subtract two values.
     */
    def subtract(r: => A)(implicit subtract: SubtractShape[A, Inverse, Associative]): A =
      subtract.subtract(l, r)

  }

}
