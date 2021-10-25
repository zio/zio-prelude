package zio.prelude
package experimental

import zio.prelude.newtypes.{Prod, Sum}

trait Annihilation[A] extends AddMultiplyShape[A] {

  override type Addition[x] <: Identity[x]

  def annihilation: A = Addition.identity
}

object Annihilation {

  type Aux[A, +addition[x] <: Identity[x], +multiplication[x] <: Associative[x]] = Annihilation[A] {
    type Addition[x] <: addition[x]
    type Multiplication[x] <: multiplication[x]
  }

  /**
   * Summons an implicit `Annihilation[A]`.
   */
  def apply[A, Addition[x] <: Identity[x], Multiplication[x] <: Associative[x]](implicit
    annihilatingZero: Annihilation.Aux[A, Addition, Multiplication]
  ): Annihilation.Aux[A, Addition, Multiplication] =
    annihilatingZero

  def fromAdditiveInverse[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    ev: DistributiveMultiply.Aux[A, Addition, Multiplication]
  ): Annihilation.Aux[A, Addition, Multiplication]
    with DistributiveMultiply.Aux[A, Addition, Multiplication]
    with SubtractShape.Aux[A, Addition, Multiplication] =
    SubtractShape.fromAdditiveInverseAndDistributiveMultiply[A, Addition, Multiplication](ev)

  def fromSubtract[A, addition[x] <: Inverse[x], multiplication[x] <: Associative[x]](implicit
    distributive0: DistributiveMultiply.Aux[A, addition, multiplication],
    subtract0: SubtractShape.Aux[A, addition, multiplication]
  ): Annihilation.Aux[A, addition, multiplication]
    with DistributiveMultiply.Aux[A, addition, multiplication]
    with SubtractShape.Aux[A, addition, multiplication] =
    new Annihilation[A] with DistributiveMultiply[A] with SubtractShape[A] {

      override type Addition[x] = addition[x]

      override type Multiplication[x] = multiplication[x]

      override def add(l: => A, r: => A): A = distributive0.add(l, r)

      override def multiply(l: => A, r: => A): A = distributive0.multiply(l, r)

      override def subtract(l: => A, r: => A): A = subtract0.subtract(l, r)

      override def Addition: addition[Sum[A]] = distributive0.Addition

      override def Multiplication: multiplication[Prod[A]] = distributive0.Multiplication
    }
}
