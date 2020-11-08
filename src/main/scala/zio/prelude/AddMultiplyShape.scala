package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait AddMultiplyShape[A, +Addition[x] <: Associative[x], +Multiplication[x] <: Associative[x]] {

  def add(l: => A, r: => A): A =
    Sum.unwrap(Addition.combine(Sum(l), Sum(r)))

  def multiply(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.combine(Prod(l), Prod(r)))

  def Addition: Addition[Sum[A]]

  def Multiplication: Multiplication[Prod[A]]
}

object AddMultiplyShape {

  /// Helper classes to make the code shorter, but we don't want them to be exposed to ZIO Prelude users
  private abstract class Ring[A]
      extends Annihilation[A, Ring.Addition, Ring.Multiplication]
      with DistributiveMultiply[A, Ring.Addition, Ring.Multiplication]
      with SubtractShape[A, Ring.Addition, Ring.Multiplication]

  private object Ring {
    type Addition[x]       = Commutative[x] with Inverse[x]
    type Multiplication[x] = Commutative[x] with Identity[x]
  }

  private abstract class Field[A]
      extends Annihilation[A, Field.Addition, Field.Multiplication]
      with DistributiveMultiply[A, Field.Addition, Field.Multiplication]
      with DivideShape[A, Field.Addition, Field.Multiplication]
      with SubtractShape[A, Field.Addition, Field.Multiplication]

  private object Field {
    type Addition[x]       = Commutative[x] with Inverse[x]
    type Multiplication[x] = Commutative[x] with InverseNonZero[x]
  }

  implicit val IntAnnihilationDistributiveMultiply: classic.Ring[Int] = new Ring[Int] {
    override def add(l: => Int, r: => Int): Int      = l + r
    override def multiply(l: => Int, r: => Int): Int = l * r
    override def subtract(l: => Int, r: => Int): Int = l - r
    override def annihilation: Int                   = 0

    override def Addition: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
      Associative.IntSumCommutativeInverse

    override def Multiplication: Commutative[Prod[Int]] with Identity[Prod[Int]] =
      Associative.IntProdCommutativeIdentity
  }

  implicit val DoubleAnnihilationDistributiveMultiply: classic.Field[Double] = new Field[Double] {
    override def add(l: => Double, r: => Double): Double      = l + r
    override def divide(l: => Double, r: => Double): Double   = l / r
    override def multiply(l: => Double, r: => Double): Double = l * r
    override def subtract(l: => Double, r: => Double): Double = l - r
    override def annihilation: Double                         = 0.0

    override def Addition: Commutative[Sum[Double]] with Inverse[Sum[Double]] =
      Associative.DoubleSumCommutativeInverse

    override def Multiplication: Commutative[Prod[Double]] with InverseNonZero[Prod[Double]] =
      Associative.DoubleProdCommutativeIdentity
  }
}

trait AddMultiplyShapeSyntax {

  /**
   * Provides infix syntax for adding or multiplying two values.
   */
  implicit class AddMultiplyShapeOps[A](private val l: A) {

    /**
     * A symbolic alias for `add`.
     */
    def +++(r: => A)(implicit associative: AddMultiplyShape[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * Add two values.
     */
    def add(r: => A)(implicit associative: AddMultiplyShape[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * A symbolic alias for `multiply`.
     */
    def ***(r: => A)(implicit associative: AddMultiplyShape[A, Associative, Associative]): A =
      associative.multiply(l, r)

    /**
     * Multiply two values.
     */
    def multiply(r: => A)(implicit associative: AddMultiplyShape[A, Associative, Associative]): A =
      associative.multiply(l, r)
  }

}
