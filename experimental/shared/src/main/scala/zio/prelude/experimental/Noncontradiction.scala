package zio.prelude
package experimental
import zio.prelude.newtypes.OrF

trait Noncontradiction[A] extends Complement[A] {
  def bottom: A = Or.identity
  def Or: Identity[OrF[A]]
}

object Noncontradiction {

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A](implicit noncontradiction: Noncontradiction[A]): Noncontradiction[A] =
    noncontradiction
}
