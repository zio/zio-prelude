package zio.prelude
package experimental

trait Involution[A] extends ExcludedMiddle[A] with Noncontradiction[A]

object Involution {

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A](implicit involution: Involution[A]): Involution[A] = involution
}
