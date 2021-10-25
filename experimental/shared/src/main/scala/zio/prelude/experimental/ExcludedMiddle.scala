package zio.prelude
package experimental

trait ExcludedMiddle[A] extends Complement[A] {

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
