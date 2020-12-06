package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }

trait AddMultiplyShape[A] {

  type Addition[x] <: Associative[x]

  type Multiplication[x] <: Associative[x]

  def add(l: => A, r: => A): A =
    Sum.unwrap(Addition.combine(Sum(l), Sum(r)))

  def multiply(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.combine(Prod(l), Prod(r)))

  def Addition: Addition[Sum[A]]

  def Multiplication: Multiplication[Prod[A]]
}

object AddMultiplyShape {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: Associative[x]] = AddMultiplyShape[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  /// Helper classes to make the code shorter, but we don't want them to be exposed to ZIO Prelude users
  private trait Ring[A] extends Annihilation[A] with DistributiveMultiply[A] with SubtractShape[A] {
    override type Addition[x] = Commutative[x] with Inverse[x]
    override type Multiplication[x] <: Commutative[x] with Identity[x]
  }

  private trait Field[A] extends Ring[A] with DivideShape[A] {
    override type Multiplication[x] = Commutative[x] with InverseNonZero[x]
  }

  implicit val IntAnnihilationDistributiveMultiply: classic.Ring[Int] = new Ring[Int] {
    override type Multiplication[x] = Commutative[x] with Identity[x]

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
    def +++(r: => A)(implicit associative: AddMultiplyShape.Aux[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * Add two values.
     */
    def add(r: => A)(implicit associative: AddMultiplyShape.Aux[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * A symbolic alias for `multiply`.
     */
    def ***(r: => A)(implicit associative: AddMultiplyShape.Aux[A, Associative, Associative]): A =
      associative.multiply(l, r)

    /**
     * Multiply two values.
     */
    def multiply(r: => A)(implicit associative: AddMultiplyShape.Aux[A, Associative, Associative]): A =
      associative.multiply(l, r)
  }

}
