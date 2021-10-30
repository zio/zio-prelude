package zio.prelude
package experimental

trait DistributiveAbsorption[A] extends Absorption[A]

object DistributiveAbsorption {

  /**
   * Summons an implicit `DistributiveAbsorption[A]`.
   */
  def apply[A](implicit distributiveAbsorption: DistributiveAbsorption[A]): DistributiveAbsorption[A] =
    distributiveAbsorption
}
