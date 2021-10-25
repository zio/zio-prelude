package zio.prelude
package experimental

trait DistributiveMultiply[A] extends AddMultiplyShape[A]

object DistributiveMultiply {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: Associative[x]] = DistributiveMultiply[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  /**
   * Summons an implicit `DistributiveMultiply[A]`.
   */
  def apply[A, Addition[x] <: Associative[x], Multiplication[x] <: Associative[x]](implicit
    distributiveMultiply: DistributiveMultiply.Aux[A, Addition, Multiplication]
  ): DistributiveMultiply.Aux[A, Addition, Multiplication] =
    distributiveMultiply
}
