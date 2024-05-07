package zio.prelude
package experimental

import zio.prelude.newtypes.{Prod, Sum}

trait DistributiveProd[A] {
  def Sum: Associative[Sum[A]]
  def Prod: Associative[Prod[A]]
  def sum(l: => A, r: => A): A  = Sum.combine(newtypes.Sum(l), newtypes.Sum(r))
  def prod(l: => A, r: => A): A = Prod.combine(newtypes.Prod(l), newtypes.Prod(r))
}

object DistributiveProd extends DistributiveProdLowPriorityImplicits {

  /**
   * Summons an implicit `DistributiveProd[A]`.
   */
  def apply[A](implicit distributiveProd: DistributiveProd[A]): DistributiveProd[A] = distributiveProd

  implicit val IntAnnihilationDistributiveProd: Annihilation[Int] with Subtract[Int] =
    new Annihilation[Int] with Subtract[Int] {
      override def sum(l: => Int, r: => Int): Int                     = l + r
      override def prod(l: => Int, r: => Int): Int                    = l * r
      override def subtract(l: => Int, r: => Int): Int                = l - r
      override def annihilation: Int                                  = 0
      val Sum: Commutative[Sum[Int]] with Inverse[Sum[Int]]           = Associative.IntSumCommutativeInverse
      val Prod: Commutative[Prod[Int]] with PartialInverse[Prod[Int]] = Associative.IntProdCommutativePartialInverse
    }

  implicit val DoubleAnnihilationDistributiveProd
    : Annihilation[Double] with PartialDivide[Double] with Subtract[Double] =
    new Annihilation[Double] with PartialDivide[Double] with Subtract[Double] {
      override def sum(l: => Double, r: => Double): Double                  = l + r
      override def divideOption(l: => Double, r: => Double): Option[Double] = if (r != 0) Some(l / r) else None
      override def prod(l: => Double, r: => Double): Double                 = l * r
      override def subtract(l: => Double, r: => Double): Double             = l - r
      override def annihilation: Double                                     = 0.0
      val Sum: Commutative[Sum[Double]] with Inverse[Sum[Double]]           = Associative.DoubleSumCommutativeInverse
      val Prod: Commutative[Prod[Double]] with PartialInverse[Prod[Double]] =
        Associative.DoubleProdCommutativePartialInverse
    }

  implicit def ZioCauseDistributiveProd[A]: DistributiveProd[zio.Cause[A]] =
    new DistributiveProd[zio.Cause[A]] {
      val Sum: Associative[Sum[zio.Cause[A]]]   = Associative.zioCauseSumCommutativeIdentity
      val Prod: Associative[Prod[zio.Cause[A]]] = Associative.zioCauseProdIdentity
    }

  implicit def ParSeqDistributiveProd[A]: DistributiveProd[ParSeq[Unit, A]] =
    new DistributiveProd[ParSeq[Unit, A]] {
      val Sum: Associative[Sum[ParSeq[Unit, A]]]   = Associative.ParSeqSumCommutativeIdentity
      val Prod: Associative[Prod[ParSeq[Unit, A]]] = Associative.ParSeqProdIdentity
    }

}

trait DistributiveProdLowPriorityImplicits {

  implicit def FxCauseDistributiveProd[A]: DistributiveProd[fx.Cause[A]] =
    new DistributiveProd[fx.Cause[A]] {
      val Sum: Associative[Sum[fx.Cause[A]]]   = Associative.FxCauseSumCommutative
      val Prod: Associative[Prod[fx.Cause[A]]] = Associative.FxCauseProdAssociative
    }

}

trait DistributiveProdSyntax {

  /**
   * Provides infix syntax for adding or multiplying two values.
   */
  implicit class DistributiveProdOps[A](private val l: A) {

    /**
     * A symbolic alias for `sum`.
     */
    def +++(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.sum(l, r)

    /**
     * Add two values.
     */
    def sum(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.sum(l, r)

    /**
     * A symbolic alias for `prod`.
     */
    def ***(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.prod(l, r)

    /**
     * Multiply two values.
     */
    def prod(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.prod(l, r)
  }
}
