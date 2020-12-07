package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._

trait AbsorptionEqual[A] extends Absorption[A, Associative, Associative] with Equal[A]

object AbsorptionEqual {
  implicit def derive[A](implicit
    absorption0: Absorption[A, Associative, Associative],
    equal0: Equal[A]
  ): AbsorptionEqual[A] =
    new AbsorptionEqual[A] {

      override def Join: Associative[A] = absorption0.Join

      override def Meet: Associative[A] = absorption0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait ComplementEqual[A] extends Complement[A, Identity, Identity] with Equal[A]

object ComplementEqual {
  implicit def derive[A](implicit
    complement0: Complement[A, Identity, Identity],
    equal0: Equal[A]
  ): ComplementEqual[A] =
    new ComplementEqual[A] {

      override def complement(a: A): A = complement0.complement(a)

      override def Join: Identity[A] = complement0.Join

      override def Meet: Identity[A] = complement0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveJoinMeetEqual[A] extends DistributiveJoinMeet[A, Associative, Associative] with Equal[A]

object DistributiveJoinMeetEqual {
  implicit def derive[A](implicit
    distributiveJoinMeet0: DistributiveJoinMeet[A, Associative, Associative],
    equal0: Equal[A]
  ): DistributiveJoinMeetEqual[A] =
    new DistributiveJoinMeetEqual[A] {

      override def Join: Associative[A] = distributiveJoinMeet0.Join

      override def Meet: Associative[A] = distributiveJoinMeet0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait InvolutionEqual[A] extends Involution[A, Associative, Associative] with Equal[A]

object InvolutionEqual {
  implicit def derive[A](implicit
    involution0: Involution[A, Associative, Associative],
    equal0: Equal[A]
  ): InvolutionEqual[A] =
    new InvolutionEqual[A] {

      override def complement(a: A): A = involution0.complement(a)

      override def Join: Associative[A] = involution0.Join

      override def Meet: Associative[A] = involution0.Meet

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
