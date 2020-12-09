package zio.prelude
package experimental

import zio.prelude.newtypes.{ AndF, OrF }

trait JoinMeetShape[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]
}

object JoinMeetShape {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = JoinMeetShape[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  implicit def BoolJoinMeet
    : Absorption[Boolean] with Complement[Boolean] with DistributiveJoinMeet[Boolean] with Involution[Boolean] =
    new Absorption[Boolean] with Complement[Boolean] with DistributiveJoinMeet[Boolean] with Involution[Boolean] {

      override type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
      override type Meet[x] = Commutative[x] with Idempotent[x] with Inverse[x]

      override def complement(a: Boolean): Boolean             = !a
      override def bottom: Boolean                             = false
      override def top: Boolean                                = true
      override def join(l: => Boolean, r: => Boolean): Boolean = l || r
      override def meet(l: => Boolean, r: => Boolean): Boolean = l && r

      override def Join: Join[OrF[Boolean]]  =
        new Commutative[OrF[Boolean]] with Idempotent[OrF[Boolean]] with Inverse[OrF[Boolean]] {
          override def inverse(l: => OrF[Boolean], r: => OrF[Boolean]): OrF[Boolean] = OrF(l && r)
          override def identity: OrF[Boolean]                                        = OrF(false)
          override def combine(l: => OrF[Boolean], r: => OrF[Boolean]): OrF[Boolean] = OrF(l || r)
        }
      override def Meet: Meet[AndF[Boolean]] =
        new Commutative[AndF[Boolean]] with Idempotent[AndF[Boolean]] with Inverse[AndF[Boolean]] {
          override def inverse(l: => AndF[Boolean], r: => AndF[Boolean]): AndF[Boolean] = AndF(l || r)
          override def identity: AndF[Boolean]                                          = AndF(true)
          override def combine(l: => AndF[Boolean], r: => AndF[Boolean]): AndF[Boolean] = AndF(l && r)
        }
    }

  implicit def SetJoinMeet[A]: Absorption[Set[A]] with DistributiveJoinMeet[Set[A]] =
    new Absorption[Set[A]] with DistributiveJoinMeet[Set[A]] {
      override type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
      override type Meet[x] = Commutative[x] with Idempotent[x]
      override def Join: Join[OrF[Set[A]]]  = Associative.SetOrFCommutativeIdempotentInverse
      override def Meet: Meet[AndF[Set[A]]] = Associative.SetAndFCommutativeIdempotent
    }
}

trait JoinMeetSyntax {

  /**
   * Provides infix syntax for adding or multiplying two values.
   */
  implicit class JoinMeetShapeOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit joinMeet: JoinMeetShape.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: JoinMeetShape.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: JoinMeetShape.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: JoinMeetShape.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)
  }

}
