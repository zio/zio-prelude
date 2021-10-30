package zio.prelude
package experimental

trait Complement[A] extends Absorption[A] {
  def complement(a: => A): A
}

object Complement {

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A](implicit complement: Complement[A]): Complement[A] = complement
}

trait ComplementSyntax {

  /**
   * Provides infix syntax for the Complement of the value.
   */
  implicit class ComplementOps[A](private val self: A)(implicit instance: Complement[A]) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_! : A =
      instance.complement(self)

    /**
     * The complement of the value.
     */
    def complement: A =
      instance.complement(self)

  }

}
