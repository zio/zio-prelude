package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._
import zio.prelude.newtypes.{AndF, OrF}

trait AbsorptionEqual[A] extends Absorption[A] with Equal[A]

object AbsorptionEqual {
  implicit def derive[A](implicit absorption0: Absorption[A], equal0: Equal[A]): AbsorptionEqual[A] =
    new AbsorptionEqual[A] {

      override def or(l: => A, r: => A): A = absorption0.or(l, r)

      override def and(l: => A, r: => A): A = absorption0.and(l, r)

      override def Or: Associative[OrF[A]] = absorption0.Or

      override def And: Associative[AndF[A]] = absorption0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait DistributiveAbsorptionEqual[A] extends AbsorptionEqual[A] with DistributiveAbsorption[A]

object DistributiveAbsorptionEqual {
  implicit def derive[A](implicit
    distributiveJoinMeet0: DistributiveAbsorption[A],
    equal0: Equal[A]
  ): DistributiveAbsorptionEqual[A] =
    new DistributiveAbsorptionEqual[A] {

      override def or(l: => A, r: => A): A = distributiveJoinMeet0.or(l, r)

      override def and(l: => A, r: => A): A = distributiveJoinMeet0.and(l, r)

      override def Or: Associative[OrF[A]] = distributiveJoinMeet0.Or

      override def And: Associative[AndF[A]] = distributiveJoinMeet0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait ExcludedMiddleEqual[A] extends AbsorptionEqual[A] with ExcludedMiddle[A]

object ExcludedMiddleEqual {
  implicit def derive[A](implicit excludedMiddle0: ExcludedMiddle[A], equal0: Equal[A]): ExcludedMiddleEqual[A] =
    new ExcludedMiddleEqual[A] {

      override def top: A = excludedMiddle0.top

      override def complement(a: => A): A = excludedMiddle0.complement(a)

      override def or(l: => A, r: => A): A = excludedMiddle0.or(l, r)

      override def and(l: => A, r: => A): A = excludedMiddle0.and(l, r)

      override def Or: Associative[OrF[A]] = excludedMiddle0.Or

      override def And: Identity[AndF[A]] = excludedMiddle0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait InvolutionEqual[A] extends AbsorptionEqual[A] with Involution[A]

object InvolutionEqual {
  implicit def derive[A](implicit involution0: Involution[A], equal0: Equal[A]): InvolutionEqual[A] =
    new InvolutionEqual[A] {

      override def complement(a: => A): A = involution0.complement(a)

      override def or(l: => A, r: => A): A = involution0.or(l, r)

      override def and(l: => A, r: => A): A = involution0.and(l, r)

      override def Or: Associative[OrF[A]] = involution0.Or

      override def And: Associative[AndF[A]] = involution0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait NoncontradictionEqual[A] extends AbsorptionEqual[A] with Noncontradiction[A]

object NoncontradictionEqual {
  implicit def derive[A](implicit noncontradiction0: Noncontradiction[A], equal0: Equal[A]): NoncontradictionEqual[A] =
    new NoncontradictionEqual[A] {

      override def bottom: A = noncontradiction0.bottom

      override def complement(a: => A): A = noncontradiction0.complement(a)

      override def or(l: => A, r: => A): A = noncontradiction0.or(l, r)

      override def and(l: => A, r: => A): A = noncontradiction0.and(l, r)

      override def Or: Identity[OrF[A]] = noncontradiction0.Or

      override def And: Associative[AndF[A]] = noncontradiction0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
