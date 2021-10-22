package zio.prelude
package experimental

trait DistributiveJoinMeet[A] extends JoinMeetShape[A]

object DistributiveJoinMeet {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = DistributiveJoinMeet[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `DistributiveJoinMeet[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    distributiveJoinMeet: DistributiveJoinMeet.Aux[A, Join, Meet]
  ): DistributiveJoinMeet.Aux[A, Join, Meet] =
    distributiveJoinMeet
}
