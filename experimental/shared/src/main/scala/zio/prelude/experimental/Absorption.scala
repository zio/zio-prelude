package zio.prelude
package experimental

import zio.prelude.newtypes._

/**
 * An experimental type class for a lattice: a type with a join (`or`) and a
 * meet (`and`) that satisfy the absorption laws, `a1 or (a1 and a2) === a1`
 * and `a1 and (a1 or a2) === a1`. Both operations are given as `Associative`
 * instances over the `OrF` and `AndF` newtypes. It lives in the experimental
 * module, so its name and shape can change between releases.
 *
 * The built-in instances are for `Boolean` and `Set[A]`. The infix syntax
 * (`vvv`, `or`, `^^^`, `and`) comes from the `zio.prelude.experimental`
 * package object, and `AbsorptionLaws` in zio-prelude-experimental-laws checks
 * the two laws.
 *
 * {{{
 * val joined: Set[Int] = Set(1, 2) vvv Set(2, 3)
 * // Set(1, 2, 3)
 * val met: Set[Int] = Set(1, 2) ^^^ Set(2, 3)
 * // Set(2)
 * }}}
 */
trait Absorption[A] {

  /**
   * Joins `l` and `r`. By default it combines them with the `Or` instance.
   */
  def or(l: => A, r: => A): A = Or.combine(OrF(l), OrF(r))

  /**
   * Meets `l` and `r`. By default it combines them with the `And` instance.
   */
  def and(l: => A, r: => A): A = And.combine(AndF(l), AndF(r))

  /**
   * The `Associative` instance for the join, over the `OrF` newtype.
   */
  def Or: Associative[OrF[A]]

  /**
   * The `Associative` instance for the meet, over the `AndF` newtype.
   */
  def And: Associative[AndF[A]]
}

/**
 * Companion of [[Absorption]], with the summoner and the built-in instances.
 */
object Absorption {

  /**
   * Summons an implicit `Absorption[A]`.
   */
  def apply[A](implicit absorption: Absorption[A]): Absorption[A] = absorption

  /**
   * The instance for `Boolean`, where `or` is `||`, `and` is `&&`,
   * `complement` is `!`, `bottom` is `false` and `top` is `true`. Because `or`
   * and `and` use `||` and `&&`, the right operand is not evaluated when the
   * left one decides the result. It is also the `Complement`,
   * `ExcludedMiddle`, `Noncontradiction` and `Involution` instance for
   * `Boolean`.
   */
  implicit lazy val BoolInstance: DistributiveAbsorption[Boolean] with Involution[Boolean] =
    new DistributiveAbsorption[Boolean] with Involution[Boolean] {
      override def complement(a: => Boolean): Boolean         = !a
      override val bottom: Boolean                            = false
      override val top: Boolean                               = true
      override def or(l: => Boolean, r: => Boolean): Boolean  = l || r
      override def and(l: => Boolean, r: => Boolean): Boolean = l && r
      override def Or: Identity[OrF[Boolean]]                 = Associative.BooleanOrFCommutativeIdempotentInverse
      override def And: Identity[AndF[Boolean]]               = Associative.BooleanAndFCommutativeIdempotentInverse
    }

  /**
   * The instance for `Set[A]`, where `or` is union and `and` is
   * intersection. There is no `Complement` instance for `Set`.
   */
  implicit def SetInstance[A]: DistributiveAbsorption[Set[A]] =
    new DistributiveAbsorption[Set[A]] {
      override def or(l: => Set[A], r: => Set[A]): Set[A]  = l | r
      override def and(l: => Set[A], r: => Set[A]): Set[A] = l & r
      override def Or: Associative[OrF[Set[A]]]            = Associative.SetOrFCommutativeIdempotentInverse
      override def And: Associative[AndF[Set[A]]]          = Associative.SetAndFCommutativeIdempotent
    }
}

/**
 * Infix syntax for [[Absorption]], mixed into the `zio.prelude.experimental`
 * package object.
 */
trait AbsorptionSyntax {

  /**
   * Provides infix syntax for joining or meeting two values.
   */
  implicit class AbsorptionOps[A](private val l: A) {

    /**
     * A symbolic alias for `or`.
     */
    def vvv(r: => A)(implicit absorption: Absorption[A]): A =
      absorption.or(l, r)

    /**
     * Joins this value with `r`, using the `or` of the `Absorption` instance.
     */
    def or(r: => A)(implicit absorption: Absorption[A]): A =
      absorption.or(l, r)

    /**
     * A symbolic alias for `and`.
     */
    def ^^^(r: => A)(implicit absorption: Absorption[A]): A =
      absorption.and(l, r)

    /**
     * Meets this value with `r`, using the `and` of the `Absorption` instance.
     */
    def and(r: => A)(implicit absorption: Absorption[A]): A =
      absorption.and(l, r)
  }

}
