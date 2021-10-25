package zio.prelude
package experimental

trait Involution[A] extends Complement[A]

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
