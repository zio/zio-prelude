package zio.prelude
package experimental

trait DivideShape[A] extends PartialDivide[A] {

  override type Multiplication[x] <: Inverse[x]

  def divide(l: => A, r: => A): A
}

object DivideShape {

  type Aux[A, +addition[x] <: Associative[x], +multiplication[x] <: Inverse[x]] = DivideShape[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

}
