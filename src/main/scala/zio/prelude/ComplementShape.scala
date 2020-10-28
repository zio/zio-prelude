package zio.prelude

trait ComplementShape[A, +Join[x] <: Associative[x], +Meet[x] <: Associative[x]] extends JoinMeet[A, Join, Meet] {
  def complement(a: A): A
}

trait ComplementShapeSyntax {

  /**
   * Provides infix syntax for the complement of the value.
   */
  implicit class ComplementShapeOps[A](private val a: A) {

    /**
     * A symbolic alias for `complement`.
     */
    def unary_!(implicit complement: ComplementShape[A, Identity, Identity]): A =
      complement.complement(a)

    /**
     * The complement of the value.
     */
    def complement(implicit complement: ComplementShape[A, Identity, Identity]): A =
      complement.complement(a)

  }

}
