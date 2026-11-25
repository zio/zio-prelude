package zio.prelude
package experimental

import zio.prelude.newtypes.Sum

/**
 * An experimental type class for a [[DistributiveProd]] whose sum has an
 * identity (the zero of the type) that also annihilates the product:
 * `annihilation *** a` and `a *** annihilation` both equal `annihilation`. It
 * lives in the experimental module, so its name and shape can change between
 * releases.
 *
 * The built-in instances are for `Byte`, `Char`, `Short`, `Int`, `Long`,
 * `Float`, `Double` and `BigDecimal`. `AnnihilationLaws` in
 * zio-prelude-experimental-laws checks the laws.
 *
 * {{{
 * val zero: Long     = Annihilation[Long].annihilation
 * // 0
 * val absorbed: Long = 7L *** zero
 * // 0
 * }}}
 */
trait Annihilation[A] extends DistributiveProd[A] {

  /**
   * The `Identity` instance for addition, over the `Sum` newtype.
   */
  def Sum: Identity[Sum[A]]

  /**
   * The element that annihilates the product. By default it is the identity
   * of `Sum`.
   */
  def annihilation: A = Sum.identity
}

/**
 * Companion of [[Annihilation]], with the summoner. The built-in instances
 * live in the `DistributiveProd` companion.
 */
object Annihilation {

  /**
   * Summons an implicit `Annihilation[A]`.
   */
  def apply[A](implicit annihilation: Annihilation[A]): Annihilation[A] = annihilation

}
