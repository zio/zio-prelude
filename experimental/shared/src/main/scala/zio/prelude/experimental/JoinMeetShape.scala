package zio.prelude
package experimental

trait JoinMeetShape[A, +Join[x] <: Associative[x], +Meet[x] <: Associative[x]] {

  def join(l: => A, r: => A): A =
    Join.combine(l, r)

  def meet(l: => A, r: => A): A =
    Meet.combine(l, r)

  def Join: Join[A]

  def Meet: Meet[A]
}

object JoinMeetShape {

  type BooleanJoin[x] = Commutative[x] with Idempotent[x] with Inverse[x]
  type BooleanMeet[x] = Commutative[x] with Idempotent[x] with Inverse[x]
  implicit def BoolJoinMeet: Absorption[Boolean, BooleanJoin, BooleanMeet]
    with Complement[Boolean, BooleanJoin, BooleanMeet]
    with DistributiveJoinMeet[Boolean, BooleanJoin, BooleanMeet]
    with Involution[Boolean, BooleanJoin, BooleanMeet] =
    new Absorption[Boolean, BooleanJoin, BooleanMeet]
      with Complement[Boolean, BooleanJoin, BooleanMeet]
      with DistributiveJoinMeet[Boolean, BooleanJoin, BooleanMeet]
      with Involution[Boolean, BooleanJoin, BooleanMeet] {

      override def complement(a: Boolean): Boolean = !a

      override def Join: BooleanJoin[Boolean] = new Commutative[Boolean]
        with Idempotent[Boolean]
        with Inverse[Boolean] {
        override def inverse(l: => Boolean, r: => Boolean): Boolean = Meet.combine(l, r)

        override def identity: Boolean = false

        override def combine(l: => Boolean, r: => Boolean): Boolean = l || r
      }
      override def Meet: BooleanMeet[Boolean] = new Commutative[Boolean]
        with Idempotent[Boolean]
        with Inverse[Boolean] {
        override def inverse(l: => Boolean, r: => Boolean): Boolean = Join.combine(l, r)

        override def identity: Boolean = true

        override def combine(l: => Boolean, r: => Boolean): Boolean = l && r
      }
    }

  type SetJoin[x] = Commutative[x] with Idempotent[x] with Inverse[x]
  type SetMeet[x] = Commutative[x] with Idempotent[x]
  implicit def SetJoinMeet[A]
    : Absorption[Set[A], SetJoin, SetMeet] with DistributiveJoinMeet[Set[A], SetJoin, SetMeet] =
    new Absorption[Set[A], SetJoin, SetMeet] with DistributiveJoinMeet[Set[A], SetJoin, SetMeet] {
      override def Join: SetJoin[Set[A]] = Associative.SetIdempotentInverse
      override def Meet: SetMeet[Set[A]] = new Commutative[Set[A]] with Idempotent[Set[A]] {
        override def combine(l: => Set[A], r: => Set[A]): Set[A] = l.intersect(r)
      }
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
    def vvv(r: => A)(implicit joinMeet: JoinMeetShape[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: JoinMeetShape[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: JoinMeetShape[A, Associative, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: JoinMeetShape[A, Associative, Associative]): A =
      joinMeet.meet(l, r)
  }

}
