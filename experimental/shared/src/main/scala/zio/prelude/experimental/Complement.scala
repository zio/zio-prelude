package zio.prelude
package experimental

/**
 * An experimental type class that adds a `complement` operation to an
 * [[Absorption]] lattice. It has no laws of its own: `ExcludedMiddle`,
 * `Noncontradiction` and `Involution` state how the complement relates to the
 * join, the meet and the bounds. It lives in the experimental module, so its
 * name and shape can change between releases.
 *
 * The built-in instance is for `Boolean`, where `complement` is `!`.
 *
 * {{{
 * val flipped: Boolean = Complement[Boolean].complement(true)
 * // false
 * }}}
 */
trait Complement[A] extends Absorption[A] {

  /**
   * Returns the complement of `a`.
   */
  def complement(a: => A): A
}

/**
 * Companion of [[Complement]], with the summoner. The `Boolean` instance lives
 * in the `Absorption` companion.
 */
object Complement {

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A](implicit complement: Complement[A]): Complement[A] = complement
}

/**
 * Infix syntax for [[Complement]], mixed into the `zio.prelude.experimental`
 * package object.
 */
trait ComplementSyntax {

  /**
   * Provides `!a` and `a.complement` for a value with a `Complement`
   * instance.
   */
  implicit class ComplementOps[A](private val self: A)(implicit instance: Complement[A]) {

    /**
     * A symbolic alias for `complement`, written `!a`. For `Boolean`, `!a`
     * calls the built-in `Boolean` operator instead, which gives the same
     * result.
     */
    def unary_! : A =
      instance.complement(self)

    /**
     * Returns the complement of this value.
     */
    def complement: A =
      instance.complement(self)

  }

}
