package zio.prelude
package experimental

trait ComplementShape[A] extends JoinMeetShape[A] {
  def complement(a: A): A
}

object ComplementShape {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = ComplementShape[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

}

trait ComplementShapeSyntax {

  /**
   * Provides infix syntax for the complement of the value.
   */
  implicit class ComplementShapeOps[A](private val a: A) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_!(implicit complement: ComplementShape.Aux[A, Identity, Identity]): A =
      complement.complement(a)

    /**
     * The complement of the value.
     */
    def complement(implicit complement: ComplementShape.Aux[A, Identity, Identity]): A =
      complement.complement(a)

  }

}
