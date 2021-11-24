package zio.prelude
package experimental
import zio.prelude.newtypes.AndF

trait ExcludedMiddle[A] extends Complement[A] {
  def top: A = And.identity
  def And: Identity[AndF[A]]
}

object ExcludedMiddle {

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A](implicit excludedMiddle: ExcludedMiddle[A]): ExcludedMiddle[A] = excludedMiddle
}
