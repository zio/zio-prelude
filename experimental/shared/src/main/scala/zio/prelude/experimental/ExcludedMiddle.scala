package zio.prelude
package experimental
import zio.prelude.newtypes.AndF

/**
 * An experimental type class for a [[Complement]] with a top element, where
 * the join of a value and its complement is the top: `!a vvv a` equals `top`.
 * It lives in the experimental module, so its name and shape can change
 * between releases.
 *
 * The built-in instance is for `Boolean`, in the `Absorption` companion.
 * `ExcludedMiddleLaws` in zio-prelude-experimental-laws checks the law.
 *
 * {{{
 * val top: Boolean = ExcludedMiddle[Boolean].top
 * // true
 * }}}
 */
trait ExcludedMiddle[A] extends Complement[A] {

  /**
   * The top element. By default it is the identity of `And`.
   */
  def top: A = And.identity

  /**
   * The `Identity` instance for the meet, over the `AndF` newtype.
   */
  def And: Identity[AndF[A]]
}

/**
 * Companion of [[ExcludedMiddle]], with the summoner.
 */
object ExcludedMiddle {

  /**
   * Summons an implicit `ExcludedMiddle[A]`.
   */
  def apply[A](implicit excludedMiddle: ExcludedMiddle[A]): ExcludedMiddle[A] = excludedMiddle
}
