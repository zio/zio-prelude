package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._
import zio.prelude.newtypes.{Prod, Sum}

trait AnnihilationEqual[A] extends Annihilation[A] with DistributiveProdEqual[A]

object AnnihilationEqual {
  implicit def derive[A](implicit annihilation0: Annihilation[A], equal0: Equal[A]): AnnihilationEqual[A] =
    new AnnihilationEqual[A] {
      override def sum(l: => A, r: => A): A         = annihilation0.sum(l, r)
      override def prod(l: => A, r: => A): A        = annihilation0.prod(l, r)
      override def Sum: Identity[Sum[A]]            = annihilation0.Sum
      override def Prod: Associative[Prod[A]]       = annihilation0.Prod
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveProdEqual[A] extends DistributiveProd[A] with Equal[A]

object DistributiveProdEqual {
  implicit def derive[A](implicit distributiveProd0: DistributiveProd[A], equal0: Equal[A]): DistributiveProdEqual[A] =
    new DistributiveProdEqual[A] {
      override def sum(l: => A, r: => A): A         = distributiveProd0.sum(l, r)
      override def prod(l: => A, r: => A): A        = distributiveProd0.prod(l, r)
      override def Sum: Associative[Sum[A]]         = distributiveProd0.Sum
      override def Prod: Associative[Prod[A]]       = distributiveProd0.Prod
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
