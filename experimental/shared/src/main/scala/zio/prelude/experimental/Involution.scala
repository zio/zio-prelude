package zio.prelude
package experimental

trait Involution[A] extends Absorption[A] {
  def complement(a: A): A
}

object Involution {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Involution[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    involution: Involution.Aux[A, Join, Meet]
  ): Involution.Aux[A, Join, Meet] =
    involution
}

trait InvolutionSyntax {

  /**
   * Provides infix syntax for the Involution of the value.
   */
  implicit class InvolutionOps[A](private val self: A)(implicit
    involution: Involution.Aux[A, Associative, Associative]
  ) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_! : A =
      involution.complement(self)

    /**
     * The complement of the value.
     */
    def complement: A =
      involution.complement(self)

  }

}
