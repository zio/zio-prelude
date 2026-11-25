package zio.prelude
package experimental
import zio.prelude.newtypes.OrF

/**
 * An experimental type class for a [[Complement]] with a bottom element,
 * where the meet of a value and its complement is the bottom: `!a ^^^ a`
 * equals `bottom`. It lives in the experimental module, so its name and shape
 * can change between releases.
 *
 * The built-in instance is for `Boolean`, in the `Absorption` companion.
 * `NoncontradictionLaws` in zio-prelude-experimental-laws checks the law.
 *
 * {{{
 * val bottom: Boolean = Noncontradiction[Boolean].bottom
 * // false
 * }}}
 */
trait Noncontradiction[A] extends Complement[A] {

  /**
   * The bottom element. By default it is the identity of `Or`.
   */
  def bottom: A = Or.identity

  /**
   * The `Identity` instance for the join, over the `OrF` newtype.
   */
  def Or: Identity[OrF[A]]
}

/**
 * Companion of [[Noncontradiction]], with the summoner.
 */
object Noncontradiction {

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A](implicit noncontradiction: Noncontradiction[A]): Noncontradiction[A] =
    noncontradiction
}
