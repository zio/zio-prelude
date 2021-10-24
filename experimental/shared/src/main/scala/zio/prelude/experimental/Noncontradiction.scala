package zio.prelude
package experimental

trait Noncontradiction[A] extends Absorption[A] {

  def complement(a: A): A

  override type Join[x] <: Identity[x]

  def bottom: A = Join.identity
}

object Noncontradiction {

  type Aux[A, +join[x] <: Identity[x], +meet[x] <: Associative[x]] = Noncontradiction[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A, Join[x] <: Identity[x], Meet[x] <: Associative[x]](implicit
    noncontradiction: Noncontradiction.Aux[A, Join, Meet]
  ): Noncontradiction.Aux[A, Join, Meet] =
    noncontradiction
}

trait NoncontradictionSyntax extends ExcludedMiddleSyntax {

  /**
   * Provides infix syntax for the Noncontradiction of the value.
   */
  implicit class NoncontradictionOps[A](private val self: A)(implicit
    noncontradiction: Noncontradiction.Aux[A, Identity, Associative]
  ) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_! : A =
      noncontradiction.complement(self)

    /**
     * The complement of the value.
     */
    def complement: A =
      noncontradiction.complement(self)

  }

}
