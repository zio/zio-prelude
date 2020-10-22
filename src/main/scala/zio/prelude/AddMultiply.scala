package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait AddMultiply[A, +Addition[x] <: Associative[x], +Multiplication[x] <: Associative[x]] {

  def add(l: => A, r: => A): A =
    Sum.unwrap(Addition.combine(Sum(l), Sum(r)))

  def multiply(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.combine(Prod(l), Prod(r)))

  def Addition: Addition[Sum[A]]

  def Multiplication: Multiplication[Prod[A]]
}

object AddMultiply extends LowPriorityAddMultiplyInstances {

  type IntAddition[x]       = Commutative[x] with Inverse[x]
  type IntMultiplication[x] = Commutative[x] with Identity[x]
  implicit val IntAnnihilatingZeroDistributiveMultiply: Subtract[Int, IntAddition, IntMultiplication]
    with AnnihilatingZero[Int, IntAddition, IntMultiplication]
    with DistributiveMultiply[Int, IntAddition, IntMultiplication] =
    new Subtract[Int, IntAddition, IntMultiplication]
      with AnnihilatingZero[Int, IntAddition, IntMultiplication]
      with DistributiveMultiply[Int, IntAddition, IntMultiplication] {

      override def add(l: => Int, r: => Int): Int = l + r

      override def multiply(l: => Int, r: => Int): Int = l * r

      override def subtract(l: => Int, r: => Int): Int = l - r

      override def Addition: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
        Associative.IntSumCommutativeInverse

      override def Multiplication: Commutative[Prod[Int]] with Identity[Prod[Int]] =
        Associative.IntProdCommutativeIdentity
    }

  type DoubleAddition[x]       = Commutative[x] with Inverse[x]
  type DoubleMultiplication[x] = Commutative[x] with InverseNonZero[x]
  implicit val DoubleAnnihilatingZeroDistributiveMultiply: Subtract[Double, DoubleAddition, DoubleMultiplication]
    with Divide[Double, DoubleAddition, DoubleMultiplication]
    with AnnihilatingZero[Double, DoubleAddition, DoubleMultiplication]
    with DistributiveMultiply[Double, DoubleAddition, DoubleMultiplication] =
    new Subtract[Double, DoubleAddition, DoubleMultiplication]
      with Divide[Double, DoubleAddition, DoubleMultiplication]
      with AnnihilatingZero[Double, DoubleAddition, DoubleMultiplication]
      with DistributiveMultiply[Double, DoubleAddition, DoubleMultiplication] {

      override def add(l: => Double, r: => Double): Double = l + r

      override def divide(l: => Double, r: => Double): Double = l / r

      override def multiply(l: => Double, r: => Double): Double = l * r

      override def subtract(l: => Double, r: => Double): Double = l - r

      override def Addition: Commutative[Sum[Double]] with Inverse[Sum[Double]] =
        Associative.DoubleSumCommutativeInverse

      override def Multiplication: Commutative[Prod[Double]] with InverseNonZero[Prod[Double]] =
        Associative.DoubleProdCommutativeIdentity
    }
}

trait LowPriorityAddMultiplyInstances {
  implicit def additiveInverseImpliesMultiplicativeZero[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[
    x
  ]](implicit
    ev: AddMultiply[A, Addition, Multiplication]
  ): AnnihilatingZero[A, Addition, Multiplication] = new AnnihilatingZero[A, Addition, Multiplication] {

    override def add(l: => A, r: => A): A = ev.add(l, r)

    override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

    override def Addition: Addition[Sum[A]] = ev.Addition

    override def Multiplication: Multiplication[Prod[A]] = ev.Multiplication
  }
}

trait AddMultiplySyntax {

  /**
   * Provides infix syntax for adding or multiplying two values.
   */
  implicit class AddMultiplyOps[A](private val l: A) {

    /**
     * A symbolic alias for `add`.
     */
    def +++(r: => A)(implicit associative: AddMultiply[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * Add two values.
     */
    def add(r: => A)(implicit associative: AddMultiply[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * A symbolic alias for `multiply`.
     */
    def ***(r: => A)(implicit associative: AddMultiply[A, Associative, Associative]): A =
      associative.multiply(l, r)

    /**
     * Multiply two values.
     */
    def multiply(r: => A)(implicit associative: AddMultiply[A, Associative, Associative]): A =
      associative.multiply(l, r)
  }

}
