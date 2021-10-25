package zio.prelude
package experimental

trait PartialDivide[A] extends DistributiveMultiply[A] {

  override type Multiplication[x] <: PartialInverse[x]

  def divideOption(l: => A, r: => A): Option[A]
}

object PartialDivide {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: PartialInverse[x]] = PartialDivide[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

}
