package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._
import zio.prelude.newtypes.{Prod, Sum}

trait AnnihilationEqual[A] extends Annihilation[A] with DistributiveMultiplyEqual[A]

object AnnihilationEqual {
  implicit def derive[A](implicit
    annihilatingZero0: Annihilation.Aux[A, Identity, Associative],
    equal0: Equal[A]
  ): AnnihilationEqual[A] =
    new AnnihilationEqual[A] {

      override type Addition[x] = Identity[x]

      override type Multiplication[x] = Associative[x]

      override def add(l: => A, r: => A): A = annihilatingZero0.add(l, r)

      override def multiply(l: => A, r: => A): A = annihilatingZero0.multiply(l, r)

      override def Addition: Identity[Sum[A]] = annihilatingZero0.Addition

      override def Multiplication: Associative[Prod[A]] = annihilatingZero0.Multiplication

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveMultiplyEqual[A] extends DistributiveMultiply[A] with Equal[A]

object DistributiveMultiplyEqual {
  implicit def derive[A](implicit
    distributive0: DistributiveMultiply.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): DistributiveMultiplyEqual[A] =
    new DistributiveMultiplyEqual[A] {

      override type Addition[x] = Associative[x]

      override type Multiplication[x] = Associative[x]

      override def add(l: => A, r: => A): A = distributive0.add(l, r)

      override def multiply(l: => A, r: => A): A = distributive0.multiply(l, r)

      override def Addition: Associative[Sum[A]] = distributive0.Addition

      override def Multiplication: Associative[Prod[A]] = distributive0.Multiplication

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
