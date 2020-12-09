package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._

trait AbsorptionEqual[A] extends Absorption[A] with Equal[A] {
  override type Join[x] = Associative[x]
  override type Meet[x] = Associative[x]
}

object AbsorptionEqual {
  implicit def derive[A](implicit
    absorption0: Absorption.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): AbsorptionEqual[A] =
    new AbsorptionEqual[A] {

      override def Join: Associative[A] = absorption0.Join

      override def Meet: Associative[A] = absorption0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait ComplementEqual[A] extends Complement[A] with Equal[A] {
  override type Join[x] = Identity[x]
  override type Meet[x] = Identity[x]
}

object ComplementEqual {
  implicit def derive[A](implicit
    complement0: Complement.Aux[A, Identity, Identity],
    equal0: Equal[A]
  ): ComplementEqual[A] =
    new ComplementEqual[A] {

      override def complement(a: A): A = complement0.complement(a)

      override def Join: Identity[A] = complement0.Join

      override def Meet: Identity[A] = complement0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveJoinMeetEqual[A] extends DistributiveJoinMeet[A] with Equal[A] {
  override type Join[x] = Associative[x]
  override type Meet[x] = Associative[x]
}

object DistributiveJoinMeetEqual {
  implicit def derive[A](implicit
    distributiveJoinMeet0: DistributiveJoinMeet.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): DistributiveJoinMeetEqual[A] =
    new DistributiveJoinMeetEqual[A] {

      override def Join: Associative[A] = distributiveJoinMeet0.Join

      override def Meet: Associative[A] = distributiveJoinMeet0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait InvolutionEqual[A] extends Involution[A] with Equal[A] {
  override type Join[x] = Associative[x]
  override type Meet[x] = Associative[x]
}

object InvolutionEqual {
  implicit def derive[A](implicit
    involution0: Involution.Aux[A, Associative, Associative],
    equal0: Equal[A]
  ): InvolutionEqual[A] =
    new InvolutionEqual[A] {

      override def complement(a: A): A = involution0.complement(a)

      override def Join: involution0.Join[A] = involution0.Join

      override def Meet: Associative[A] = involution0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
