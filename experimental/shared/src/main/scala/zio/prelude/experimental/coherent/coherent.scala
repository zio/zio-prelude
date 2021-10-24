package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._
import zio.prelude.newtypes.{AndF, OrF}

trait AbsorptionEqual[A] extends Absorption[A] with Equal[A]

object AbsorptionEqual {
  implicit def derive[A](implicit
    absorption0: Absorption.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): AbsorptionEqual[A] =
    new AbsorptionEqual[A] {

      override type Join[x] = Associative[x]

      override type Meet[x] = Associative[x]

      override def Join: Associative[OrF[A]] = absorption0.Join

      override def Meet: Associative[AndF[A]] = absorption0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveAbsorptionEqual[A] extends AbsorptionEqual[A] with DistributiveAbsorption[A]

object DistributiveAbsorptionEqual {
  implicit def derive[A](implicit
    distributiveJoinMeet0: DistributiveAbsorption.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): DistributiveAbsorptionEqual[A] =
    new DistributiveAbsorptionEqual[A] {

      override type Join[x] = Associative[x]

      override type Meet[x] = Associative[x]

      override def Join: Associative[OrF[A]] = distributiveJoinMeet0.Join

      override def Meet: Associative[AndF[A]] = distributiveJoinMeet0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait ExcludedMiddleEqual[A] extends AbsorptionEqual[A] with ExcludedMiddle[A]

object ExcludedMiddleEqual {
  implicit def derive[A](implicit
    excludedMiddle0: ExcludedMiddle.Aux[A, Associative, Identity],
    equal0: Equal[A]
  ): ExcludedMiddleEqual[A] =
    new ExcludedMiddleEqual[A] {

      override type Join[x] = Associative[x]

      override type Meet[x] = Identity[x]

      override def complement(a: A): A = excludedMiddle0.complement(a)

      override def Join: Associative[OrF[A]] = excludedMiddle0.Join

      override def Meet: Identity[AndF[A]] = excludedMiddle0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait InvolutionEqual[A] extends AbsorptionEqual[A] with Involution[A]

object InvolutionEqual {
  implicit def derive[A](implicit
    involution0: Involution.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): InvolutionEqual[A] =
    new InvolutionEqual[A] {

      override type Join[x] = Associative[x]

      override type Meet[x] = Associative[x]

      override def complement(a: A): A = involution0.complement(a)

      override def Join: Join[OrF[A]] = involution0.Join

      override def Meet: Associative[AndF[A]] = involution0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait NoncontradictionEqual[A] extends AbsorptionEqual[A] with Noncontradiction[A]

object NoncontradictionEqual {
  implicit def derive[A](implicit
    noncontradiction0: Noncontradiction.Aux[A, Identity, Associative],
    equal0: Equal[A]
  ): NoncontradictionEqual[A] =
    new NoncontradictionEqual[A] {

      override type Join[x] = Identity[x]

      override type Meet[x] = Associative[x]

      override def complement(a: A): A = noncontradiction0.complement(a)

      override def Join: Identity[OrF[A]] = noncontradiction0.Join

      override def Meet: Associative[AndF[A]] = noncontradiction0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
