package zio.prelude

trait JoinMeet[A, +Join[x] <: Associative[x], +Meet[x] <: Associative[x]] {

  def join(l: => A, r: => A): A =
    Join.combine(l, r)

  def meet(l: => A, r: => A): A =
    Meet.combine(l, r)

  def Join: Join[A]

  def Meet: Meet[A]
}

object JoinMeet {

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
  implicit class JoinMeetOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit associative: JoinMeet[A, Associative, Associative]): A =
      associative.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit associative: JoinMeet[A, Associative, Associative]): A =
      associative.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit associative: JoinMeet[A, Associative, Associative]): A =
      associative.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit associative: JoinMeet[A, Associative, Associative]): A =
      associative.meet(l, r)
  }

}
