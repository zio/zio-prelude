package zio.prelude
package experimental

trait Noncontradiction[A] extends Complement[A] {

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
