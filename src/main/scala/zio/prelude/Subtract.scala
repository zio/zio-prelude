package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait Subtract[A, +Addition[x] <: Inverse[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiply[A, Addition, Multiplication] {

  def subtract(l: => A, r: => A): A =
    Sum.unwrap(Addition.inverse(Sum(l), Sum(r)))
}

object Subtract {

  def fromAdditiveInverse[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    ev: AddMultiply[A, Addition, Multiplication]
  ): Subtract[A, Addition, Multiplication] = new Subtract[A, Addition, Multiplication] {

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: Addition[Sum[A]] = ev.Addition

    override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
  }

  def fromAdditiveInverseAndDistributive[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    ev: Distributive[A, Addition, Multiplication]
  ): AnnihilatingZero[A, Addition, Multiplication]
    with Distributive[A, Addition, Multiplication]
    with Subtract[A, Addition, Multiplication] =
    new AnnihilatingZero[A, Addition, Multiplication]
      with Distributive[A, Addition, Multiplication]
      with Subtract[A, Addition, Multiplication] {

      override def add(l: => A, r: => A): A = ev.add(l, r)

      override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

      override def Addition: Addition[Sum[A]] = ev.Addition

      override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
    }
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
