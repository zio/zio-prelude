package zio.prelude
package experimental

trait DistributiveAbsorption[A] extends Absorption[A]

object DistributiveAbsorption {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = DistributiveAbsorption[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `DistributiveAbsorption[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    distributiveAbsorption: DistributiveAbsorption.Aux[A, Join, Meet]
  ): DistributiveAbsorption.Aux[A, Join, Meet] =
    distributiveAbsorption
}
