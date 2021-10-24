package zio.prelude
package experimental

trait ExcludedMiddle[A] extends Absorption[A] {

  def complement(a: A): A

  override type Meet[x] <: Identity[x]

  def top: A = Meet.identity
}

object ExcludedMiddle {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Identity[x]] = ExcludedMiddle[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Identity[x]](implicit
    excludedMiddle: ExcludedMiddle.Aux[A, Join, Meet]
  ): ExcludedMiddle.Aux[A, Join, Meet] =
    excludedMiddle
}

trait ExcludedMiddleSyntax extends InvolutionSyntax {

  /**
   * Provides infix syntax for the ExcludedMiddle of the value.
   */
  implicit class ExcludedMiddleOps[A](private val self: A)(implicit
    excludedMiddle: ExcludedMiddle.Aux[A, Associative, Identity]
  ) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_! : A =
      excludedMiddle.complement(self)

    /**
     * The complement of the value.
     */
    def complement: A =
      excludedMiddle.complement(self)

  }

}
