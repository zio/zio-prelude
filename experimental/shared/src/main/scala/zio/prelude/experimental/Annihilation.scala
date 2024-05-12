package zio.prelude
package experimental

import zio.prelude.newtypes.Sum

trait Annihilation[A] extends DistributiveProd[A] {
  def Sum: Identity[Sum[A]]
  def annihilation: A = Sum.identity
}

object Annihilation {

  /**
   * Summons an implicit `Annihilation[A]`.
   */
  def apply[A](implicit annihilation: Annihilation[A]): Annihilation[A] = annihilation

}
