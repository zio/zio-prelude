package zio.prelude
package experimental

import zio.prelude.newtypes.{Prod, Sum}

trait AddMultiplyShape[A] {

  type Addition[x] <: Associative[x]

  type Multiplication[x] <: Associative[x]

  def add(l: => A, r: => A): A =
    Addition.combine(Sum(l), Sum(r))

  def multiply(l: => A, r: => A): A =
    Multiplication.combine(Prod(l), Prod(r))

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

  private trait Field[A] extends Ring[A] with PartialDivideShape[A] {
    override type Multiplication[x] = Commutative[x] with PartialInverse[x]
  }

  implicit val IntAnnihilationDistributiveMultiply: classic.Ring[Int] = new Ring[Int] {
    override type Multiplication[x] = Commutative[x] with Identity[x]

    override def add(l: => Int, r: => Int): Int      = l + r
    override def multiply(l: => Int, r: => Int): Int = l * r
    override def subtract(l: => Int, r: => Int): Int = l - r
    override def annihilation: Int                   = 0

    val Addition: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
      Associative.IntSumCommutativeInverse

    val Multiplication: Commutative[Prod[Int]] with PartialInverse[Prod[Int]] =
      Associative.IntProdCommutativePartialInverse
  }

  implicit val DoubleAnnihilationDistributiveMultiply: classic.Field[Double] = new Field[Double] {
    override def add(l: => Double, r: => Double): Double                  = l + r
    override def divideOption(l: => Double, r: => Double): Option[Double] = if (r != 0) Some(l / r) else None
    override def multiply(l: => Double, r: => Double): Double             = l * r
    override def subtract(l: => Double, r: => Double): Double             = l - r
    override def annihilation: Double                                     = 0.0

    val Addition: Commutative[Sum[Double]] with Inverse[Sum[Double]] =
      Associative.DoubleSumCommutativeInverse

    val Multiplication: Commutative[Prod[Double]] with PartialInverse[Prod[Double]] =
      Associative.DoubleProdCommutativePartialInverse
  }

  implicit def ZioCauseDistributiveMultiply[A]: DistributiveMultiply[zio.Cause[A]] =
    new DistributiveMultiply[zio.Cause[A]] {
      type Addition[x]       = Commutative[x] with Identity[x]
      type Multiplication[x] = Identity[x]

      val Addition: Addition[Sum[zio.Cause[A]]] = new Commutative[Sum[zio.Cause[A]]] with Identity[Sum[zio.Cause[A]]] {
        def identity: Sum[zio.Cause[A]]                                                  = Sum(zio.Cause.empty)
        def combine(l: => Sum[zio.Cause[A]], r: => Sum[zio.Cause[A]]): Sum[zio.Cause[A]] = Sum(l.&&[A](r))
      }

      val Multiplication: Multiplication[Prod[zio.Cause[A]]] = new Identity[Prod[zio.Cause[A]]] {
        def identity: Prod[zio.Cause[A]]                                                    = Prod(zio.Cause.empty)
        def combine(l: => Prod[zio.Cause[A]], r: => Prod[zio.Cause[A]]): Prod[zio.Cause[A]] = Prod(l.++[A](r))
      }
    }

  implicit def ParSeqDistributiveMultiply[A]: DistributiveMultiply[ParSeq[Unit, A]] =
    new DistributiveMultiply[ParSeq[Unit, A]] {
      type Addition[x]       = Commutative[x] with Identity[x]
      type Multiplication[x] = Identity[x]

      val Addition: Addition[Sum[ParSeq[Unit, A]]] = new Commutative[Sum[ParSeq[Unit, A]]]
        with Identity[Sum[ParSeq[Unit, A]]] {
        def identity: Sum[ParSeq[Unit, A]]                                                        =
          Sum(ParSeq.empty)
        def combine(l: => Sum[ParSeq[Unit, A]], r: => Sum[ParSeq[Unit, A]]): Sum[ParSeq[Unit, A]] =
          Sum(l.&&[Unit, A](r))
      }

      val Multiplication: Multiplication[Prod[ParSeq[Unit, A]]] = new Identity[Prod[ParSeq[Unit, A]]] {
        def identity: Prod[ParSeq[Unit, A]]                                                          =
          Prod(ParSeq.empty)
        def combine(l: => Prod[ParSeq[Unit, A]], r: => Prod[ParSeq[Unit, A]]): Prod[ParSeq[Unit, A]] =
          Prod(l.++[Unit, A](r))
      }
    }

  implicit def FxCauseDistributiveMultiply[A]: DistributiveMultiply[fx.Cause[A]] =
    new DistributiveMultiply[fx.Cause[A]] {
      type Addition[x]       = Commutative[x]
      type Multiplication[x] = Associative[x]

      val Addition: Addition[Sum[fx.Cause[A]]] = new Commutative[Sum[fx.Cause[A]]] {
        def combine(l: => Sum[fx.Cause[A]], r: => Sum[fx.Cause[A]]): Sum[fx.Cause[A]] = Sum(l.&&[Nothing, A](r))
      }

      val Multiplication: Multiplication[Prod[fx.Cause[A]]] = new Associative[Prod[fx.Cause[A]]] {
        def combine(l: => Prod[fx.Cause[A]], r: => Prod[fx.Cause[A]]): Prod[fx.Cause[A]] = Prod(l.++[Nothing, A](r))
      }
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
