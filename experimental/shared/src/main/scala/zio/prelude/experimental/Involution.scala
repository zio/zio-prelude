package zio.prelude
package experimental

/**
 * An experimental type class for a Boolean algebra: an [[ExcludedMiddle]] and
 * [[Noncontradiction]] instance whose complement is an involution, so that
 * `complement(complement(a))` equals `a`. It adds no members. It lives in the
 * experimental module, so its name and shape can change between releases.
 *
 * The built-in instance is for `Boolean`, in the `Absorption` companion.
 * `InvolutionLaws` in zio-prelude-experimental-laws checks the laws.
 *
 * {{{
 * val boolean = Involution[Boolean]
 * val same: Boolean = boolean.complement(boolean.complement(false))
 * // false
 * }}}
 */
trait Involution[A] extends ExcludedMiddle[A] with Noncontradiction[A]

/**
 * Companion of [[Involution]], with the summoner.
 */
object Involution {

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A](implicit involution: Involution[A]): Involution[A] = involution
}
