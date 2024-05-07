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

  implicit val BigDecimalAnnihilationPartialDivideSubtract
    : Annihilation[BigDecimal] with PartialDivide[BigDecimal] with Subtract[BigDecimal] =
    new Annihilation[BigDecimal] with PartialDivide[BigDecimal] with Subtract[BigDecimal] {
      override def sum(l: => BigDecimal, r: => BigDecimal): BigDecimal                  = l + r
      override def divideOption(l: => BigDecimal, r: => BigDecimal): Option[BigDecimal] =
        if (r != BigDecimal(0)) Some(l / r) else None
      override def prod(l: => BigDecimal, r: => BigDecimal): BigDecimal                 = l * r
      override def subtract(l: => BigDecimal, r: => BigDecimal): BigDecimal             = l - r
      override def annihilation: BigDecimal                                             = 0.0
      val Sum: Commutative[Sum[BigDecimal]] with Inverse[Sum[BigDecimal]]               = Associative.BigDecimalSumCommutativeInverse
      val Prod: Commutative[Prod[BigDecimal]] with PartialInverse[Prod[BigDecimal]]     =
        Associative.BigDecimalProdCommutativePartialInverse
    }

  implicit val ByteAnnihilationSubtract: Annihilation[Byte] with Subtract[Byte] =
    new Annihilation[Byte] with Subtract[Byte] {
      override def sum(l: => Byte, r: => Byte): Byte                    = (l + r).toByte
      override def prod(l: => Byte, r: => Byte): Byte                   = (l * r).toByte
      override def subtract(l: => Byte, r: => Byte): Byte               = (l - r).toByte
      override def annihilation: Byte                                   = 0
      val Sum: Commutative[Sum[Byte]] with Inverse[Sum[Byte]]           = Associative.ByteSumCommutativeInverse
      val Prod: Commutative[Prod[Byte]] with PartialInverse[Prod[Byte]] = Associative.ByteProdCommutativePartialInverse
    }

  implicit val CharAnnihilationSubtract: Annihilation[Char] with Subtract[Char] =
    new Annihilation[Char] with Subtract[Char] {
      override def sum(l: => Char, r: => Char): Char                    = (l + r).toChar
      override def prod(l: => Char, r: => Char): Char                   = (l * r).toChar
      override def subtract(l: => Char, r: => Char): Char               = (l - r).toChar
      override def annihilation: Char                                   = 0
      val Sum: Commutative[Sum[Char]] with Inverse[Sum[Char]]           = Associative.CharSumCommutativeInverse
      val Prod: Commutative[Prod[Char]] with PartialInverse[Prod[Char]] = Associative.CharProdCommutativePartialInverse
    }

  implicit val DoubleAnnihilationPartialDivideSubtract
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

  implicit val FloatAnnihilationPartialDivideSubtract
    : Annihilation[Float] with PartialDivide[Float] with Subtract[Float] =
    new Annihilation[Float] with PartialDivide[Float] with Subtract[Float] {
      override def sum(l: => Float, r: => Float): Float                   = l + r
      override def divideOption(l: => Float, r: => Float): Option[Float]  = if (r != 0) Some(l / r) else None
      override def prod(l: => Float, r: => Float): Float                  = l * r
      override def subtract(l: => Float, r: => Float): Float              = l - r
      override def annihilation: Float                                    = 0.0f
      val Sum: Commutative[Sum[Float]] with Inverse[Sum[Float]]           = Associative.FloatSumCommutativeInverse
      val Prod: Commutative[Prod[Float]] with PartialInverse[Prod[Float]] =
        Associative.FloatProdCommutativePartialInverse
    }

  implicit val IntAnnihilationSubtract: Annihilation[Int] with Subtract[Int] =
    new Annihilation[Int] with Subtract[Int] {
      override def sum(l: => Int, r: => Int): Int                     = l + r
      override def prod(l: => Int, r: => Int): Int                    = l * r
      override def subtract(l: => Int, r: => Int): Int                = l - r
      override def annihilation: Int                                  = 0
      val Sum: Commutative[Sum[Int]] with Inverse[Sum[Int]]           = Associative.IntSumCommutativeInverse
      val Prod: Commutative[Prod[Int]] with PartialInverse[Prod[Int]] = Associative.IntProdCommutativePartialInverse
    }

  implicit val LongAnnihilationSubtract: Annihilation[Long] with Subtract[Long] =
    new Annihilation[Long] with Subtract[Long] {
      override def sum(l: => Long, r: => Long): Long                    = l + r
      override def prod(l: => Long, r: => Long): Long                   = l * r
      override def subtract(l: => Long, r: => Long): Long               = l - r
      override def annihilation: Long                                   = 0
      val Sum: Commutative[Sum[Long]] with Inverse[Sum[Long]]           = Associative.LongSumCommutativeInverse
      val Prod: Commutative[Prod[Long]] with PartialInverse[Prod[Long]] = Associative.LongProdCommutativePartialInverse
    }

  implicit def ParSeqDistributiveProd[A]: DistributiveProd[ParSeq[Unit, A]] =
    new DistributiveProd[ParSeq[Unit, A]] {
      val Sum: Associative[Sum[ParSeq[Unit, A]]]   = Associative.ParSeqSumCommutativeIdentity
      val Prod: Associative[Prod[ParSeq[Unit, A]]] = Associative.ParSeqProdIdentity
    }

  implicit val ShortAnnihilationSubtract: Annihilation[Short] with Subtract[Short] =
    new Annihilation[Short] with Subtract[Short] {
      override def sum(l: => Short, r: => Short): Short                   = (l + r).toShort
      override def prod(l: => Short, r: => Short): Short                  = (l * r).toShort
      override def subtract(l: => Short, r: => Short): Short              = (l - r).toShort
      override def annihilation: Short                                    = 0
      val Sum: Commutative[Sum[Short]] with Inverse[Sum[Short]]           = Associative.ShortSumCommutativeInverse
      val Prod: Commutative[Prod[Short]] with PartialInverse[Prod[Short]] =
        Associative.ShortProdCommutativePartialInverse
    }

  implicit def ZioCauseDistributiveProd[A]: DistributiveProd[zio.Cause[A]] =
    new DistributiveProd[zio.Cause[A]] {
      val Sum: Associative[Sum[zio.Cause[A]]]   = Associative.zioCauseSumCommutativeIdentity
      val Prod: Associative[Prod[zio.Cause[A]]] = Associative.zioCauseProdIdentity
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
