package zio.prelude
package experimental

trait Subtract[A] extends DistributiveMultiply[A] {

  override type Addition[x] <: Inverse[x]

  def subtract(l: => A, r: => A): A
}

object Subtract {

  type Aux[A, +addition[x] <: Inverse[x], +multiplication[x] <: Associative[x]] = Subtract[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

}
