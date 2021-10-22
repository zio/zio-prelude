package zio.prelude
package experimental

trait Absorption[A] extends JoinMeetShape[A]

object Absorption {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Absorption[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Absorption[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    absorption: Absorption.Aux[A, Join, Meet]
  ): Absorption.Aux[A, Join, Meet] =
    absorption
}
