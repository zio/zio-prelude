package zio.prelude
package experimental

trait Complement[A] extends Absorption[A] {
  def complement(a: A): A
}

object Complement {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Complement[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    complement: Complement.Aux[A, Join, Meet]
  ): Complement.Aux[A, Join, Meet] =
    complement
}

trait ComplementSyntax {

  /**
   * Provides infix syntax for the Complement of the value.
   */
  implicit class ComplementOps[A](private val self: A)(implicit instance: Complement.Aux[A, Associative, Associative]) {

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
