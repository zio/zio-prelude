package zio.prelude
package experimental

import zio.prelude.newtypes.{Prod, Sum}

trait Annihilation[A] extends DistributiveMultiply[A] {

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

  def fromAdditiveInverse[A, addition[x] <: Inverse[x], multiplication[x] <: Associative[x]](implicit
    ev: DistributiveMultiply.Aux[A, addition, multiplication]
  ): Annihilation.Aux[A, addition, multiplication] =
    new Annihilation[A] {

      override type Addition[x] = addition[x]

      override type Multiplication[x] = multiplication[x]

      override def add(l: => A, r: => A): A = ev.add(l, r)

      override def multiply(l: => A, r: => A): A = ev.multiply(l, r)

      override def Addition: addition[Sum[A]] = ev.Addition

      override def Multiplication: multiplication[Prod[A]] = ev.Multiplication
    }

}
