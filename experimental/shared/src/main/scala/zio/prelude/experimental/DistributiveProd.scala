package zio.prelude
package experimental

import zio.prelude.newtypes.{Prod, Sum}

/**
 * An experimental type class for a type with an addition (`sum`) and a
 * multiplication (`prod`) that distributes over it on both sides:
 * `a1 *** (a2 +++ a3)` equals `(a1 *** a2) +++ (a1 *** a3)`, and
 * `(a1 +++ a2) *** a3` equals `(a1 *** a3) +++ (a2 *** a3)`. Both operations
 * are given as `Associative` instances over the `Sum` and `Prod` newtypes. It
 * lives in the experimental module, so its name and shape can change between
 * releases.
 *
 * The built-in instances are for `Byte`, `Char`, `Short`, `Int`, `Long`,
 * `Float`, `Double` and `BigDecimal` (all of them [[Annihilation]] and
 * [[Subtract]] instances too), and for `ParSeq[Unit, A]` and `zio.Cause[A]`,
 * where `sum` is parallel composition and `prod` is sequential composition.
 * The infix syntax (`+++`, `sum`, `***`, `prod`) comes from the
 * `zio.prelude.experimental` package object, and `DistributiveProdLaws` in
 * zio-prelude-experimental-laws checks the two laws.
 *
 * {{{
 * val total: Int   = 3 +++ 4
 * // 7
 * val product: Int = 3 *** 4
 * // 12
 * }}}
 */
trait DistributiveProd[A] {

  /**
   * The `Associative` instance for addition, over the `Sum` newtype.
   */
  def Sum: Associative[Sum[A]]

  /**
   * The `Associative` instance for multiplication, over the `Prod` newtype.
   */
  def Prod: Associative[Prod[A]]

  /**
   * Adds `l` and `r`. By default it combines them with the `Sum` instance.
   */
  def sum(l: => A, r: => A): A = Sum.combine(newtypes.Sum(l), newtypes.Sum(r))

  /**
   * Multiplies `l` and `r`. By default it combines them with the `Prod`
   * instance.
   */
  def prod(l: => A, r: => A): A = Prod.combine(newtypes.Prod(l), newtypes.Prod(r))
}

/**
 * Companion of [[DistributiveProd]], with the summoner and the built-in
 * instances.
 */
object DistributiveProd extends DistributiveProdLowPriorityImplicits {

  /**
   * Summons an implicit `DistributiveProd[A]`.
   */
  def apply[A](implicit distributiveProd: DistributiveProd[A]): DistributiveProd[A] = distributiveProd

  /**
   * The instance for `BigDecimal`, with `+`, `*`, `-` and `/`.
   * `divideOption` returns `None` when the divisor equals zero.
   */
  implicit lazy val BigDecimalAnnihilationPartialDivideSubtract
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

  /**
   * The instance for `Byte`. `sum`, `prod` and `subtract` compute on `Int`
   * and convert back with `toByte`, so results outside the `Byte` range wrap
   * around: `(120: Byte) +++ (10: Byte)` is `-126`.
   */
  implicit lazy val ByteAnnihilationSubtract: Annihilation[Byte] with Subtract[Byte] =
    new Annihilation[Byte] with Subtract[Byte] {
      override def sum(l: => Byte, r: => Byte): Byte                    = (l + r).toByte
      override def prod(l: => Byte, r: => Byte): Byte                   = (l * r).toByte
      override def subtract(l: => Byte, r: => Byte): Byte               = (l - r).toByte
      override def annihilation: Byte                                   = 0
      val Sum: Commutative[Sum[Byte]] with Inverse[Sum[Byte]]           = Associative.ByteSumCommutativeInverse
      val Prod: Commutative[Prod[Byte]] with PartialInverse[Prod[Byte]] = Associative.ByteProdCommutativePartialInverse
    }

  /**
   * The instance for `Char`, using its numeric code. `sum`, `prod` and
   * `subtract` compute on `Int` and convert back with `toChar`, so results
   * outside the `Char` range wrap around: `'a' --- 'b'` is the `Char` with
   * code 65535.
   */
  implicit lazy val CharAnnihilationSubtract: Annihilation[Char] with Subtract[Char] =
    new Annihilation[Char] with Subtract[Char] {
      override def sum(l: => Char, r: => Char): Char                    = (l + r).toChar
      override def prod(l: => Char, r: => Char): Char                   = (l * r).toChar
      override def subtract(l: => Char, r: => Char): Char               = (l - r).toChar
      override def annihilation: Char                                   = 0
      val Sum: Commutative[Sum[Char]] with Inverse[Sum[Char]]           = Associative.CharSumCommutativeInverse
      val Prod: Commutative[Prod[Char]] with PartialInverse[Prod[Char]] = Associative.CharProdCommutativePartialInverse
    }

  /**
   * The instance for `Double`, with `+`, `*`, `-` and `/`. `divideOption`
   * returns `None` when the divisor is `0.0` or `-0.0`. Because of rounding,
   * the distributive laws hold for `Double` only approximately.
   */
  implicit lazy val DoubleAnnihilationPartialDivideSubtract
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

  /**
   * The instance for `Float`, with `+`, `*`, `-` and `/`. `divideOption`
   * returns `None` when the divisor is `0.0f` or `-0.0f`. Because of rounding,
   * the distributive laws hold for `Float` only approximately.
   */
  implicit lazy val FloatAnnihilationPartialDivideSubtract
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

  /**
   * The instance for `Int`, with `+`, `*` and `-`, which overflow as `Int`
   * arithmetic does. There is no `PartialDivide` instance for `Int`.
   */
  implicit lazy val IntAnnihilationSubtract: Annihilation[Int] with Subtract[Int] =
    new Annihilation[Int] with Subtract[Int] {
      override def sum(l: => Int, r: => Int): Int                     = l + r
      override def prod(l: => Int, r: => Int): Int                    = l * r
      override def subtract(l: => Int, r: => Int): Int                = l - r
      override def annihilation: Int                                  = 0
      val Sum: Commutative[Sum[Int]] with Inverse[Sum[Int]]           = Associative.IntSumCommutativeInverse
      val Prod: Commutative[Prod[Int]] with PartialInverse[Prod[Int]] = Associative.IntProdCommutativePartialInverse
    }

  /**
   * The instance for `Long`, with `+`, `*` and `-`, which overflow as `Long`
   * arithmetic does. There is no `PartialDivide` instance for `Long`.
   */
  implicit lazy val LongAnnihilationSubtract: Annihilation[Long] with Subtract[Long] =
    new Annihilation[Long] with Subtract[Long] {
      override def sum(l: => Long, r: => Long): Long                    = l + r
      override def prod(l: => Long, r: => Long): Long                   = l * r
      override def subtract(l: => Long, r: => Long): Long               = l - r
      override def annihilation: Long                                   = 0
      val Sum: Commutative[Sum[Long]] with Inverse[Sum[Long]]           = Associative.LongSumCommutativeInverse
      val Prod: Commutative[Prod[Long]] with PartialInverse[Prod[Long]] = Associative.LongProdCommutativePartialInverse
    }

  /**
   * The instance for `ParSeq[Unit, A]`, where `sum` is parallel composition
   * (`&&`, giving `Both`) and `prod` is sequential composition (`++`, giving
   * `Then`). `Sum` and `Prod` are typed as `Associative`; the values behind
   * them are `Identity` instances whose identity is the empty `ParSeq`. The
   * operands have to be typed as `ParSeq[Unit, A]`: `ParSeq.single(1)` alone
   * is inferred as `ParSeq[Nothing, Int]`, which has no instance.
   */
  implicit def ParSeqDistributiveProd[A]: DistributiveProd[ParSeq[Unit, A]] =
    new DistributiveProd[ParSeq[Unit, A]] {
      val Sum: Associative[Sum[ParSeq[Unit, A]]]   = Associative.ParSeqSumCommutativeIdentity
      val Prod: Associative[Prod[ParSeq[Unit, A]]] = Associative.ParSeqProdIdentity
    }

  /**
   * The instance for `Short`. `sum`, `prod` and `subtract` compute on `Int`
   * and convert back with `toShort`, so results outside the `Short` range
   * wrap around.
   */
  implicit lazy val ShortAnnihilationSubtract: Annihilation[Short] with Subtract[Short] =
    new Annihilation[Short] with Subtract[Short] {
      override def sum(l: => Short, r: => Short): Short                   = (l + r).toShort
      override def prod(l: => Short, r: => Short): Short                  = (l * r).toShort
      override def subtract(l: => Short, r: => Short): Short              = (l - r).toShort
      override def annihilation: Short                                    = 0
      val Sum: Commutative[Sum[Short]] with Inverse[Sum[Short]]           = Associative.ShortSumCommutativeInverse
      val Prod: Commutative[Prod[Short]] with PartialInverse[Prod[Short]] =
        Associative.ShortProdCommutativePartialInverse
    }

  /**
   * The instance for `zio.Cause[A]`, where `sum` is parallel composition
   * (`&&`, giving `Both`) and `prod` is sequential composition (`++`, giving
   * `Then`). `Sum` and `Prod` are typed as `Associative`; the values behind
   * them are `Identity` instances whose identity is `Cause.empty`.
   */
  implicit def ZioCauseDistributiveProd[A]: DistributiveProd[zio.Cause[A]] =
    new DistributiveProd[zio.Cause[A]] {
      val Sum: Associative[Sum[zio.Cause[A]]]   = Associative.zioCauseSumCommutativeIdentity
      val Prod: Associative[Prod[zio.Cause[A]]] = Associative.zioCauseProdIdentity
    }

}

/**
 * Lower-priority `DistributiveProd` instances, inherited by the
 * `DistributiveProd` companion.
 */
trait DistributiveProdLowPriorityImplicits {

  /**
   * An instance for `ParSeq[Unit, A]` built from instances without an
   * identity (`Associative.parSeqSumCommutative` and
   * `Associative.parSeqProdAssociative`). It has the same type as
   * `ParSeqDistributiveProd` in the `DistributiveProd` companion, which takes
   * priority, so implicit search selects `ParSeqDistributiveProd`.
   */
  implicit def parSeqDistributiveProd[A]: DistributiveProd[ParSeq[Unit, A]] =
    new DistributiveProd[ParSeq[Unit, A]] {
      val Sum: Associative[Sum[ParSeq[Unit, A]]]   = Associative.parSeqSumCommutative
      val Prod: Associative[Prod[ParSeq[Unit, A]]] = Associative.parSeqProdAssociative
    }

}

/**
 * Infix syntax for [[DistributiveProd]], mixed into the
 * `zio.prelude.experimental` package object.
 */
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
     * Adds this value and `r`, using the `sum` of the `DistributiveProd`
     * instance.
     */
    def sum(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.sum(l, r)

    /**
     * A symbolic alias for `prod`.
     */
    def ***(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.prod(l, r)

    /**
     * Multiplies this value by `r`, using the `prod` of the `DistributiveProd`
     * instance.
     */
    def prod(r: => A)(implicit distributiveProd: DistributiveProd[A]): A =
      distributiveProd.prod(l, r)
  }
}
