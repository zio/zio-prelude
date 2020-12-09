package zio.prelude
package experimental

trait JoinMeetShape[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(l, r)

  def meet(l: => A, r: => A): A =
    Meet.combine(l, r)

  def Join: Join[A]

  def Meet: Meet[A]
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

      override def complement(a: Boolean): Boolean = !a

      override def Join: Join[Boolean] = new Commutative[Boolean] with Idempotent[Boolean] with Inverse[Boolean] {
        override def inverse(l: => Boolean, r: => Boolean): Boolean = Meet.combine(l, r)

        override def identity: Boolean = false

        override def combine(l: => Boolean, r: => Boolean): Boolean = l || r
      }
      override def Meet: Meet[Boolean] = new Commutative[Boolean] with Idempotent[Boolean] with Inverse[Boolean] {
        override def inverse(l: => Boolean, r: => Boolean): Boolean = Join.combine(l, r)

        override def identity: Boolean = true

        override def combine(l: => Boolean, r: => Boolean): Boolean = l && r
      }
    }

  implicit def SetJoinMeet[A]: Absorption[Set[A]] with DistributiveJoinMeet[Set[A]] =
    new Absorption[Set[A]] with DistributiveJoinMeet[Set[A]] {
      override type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
      override type Meet[x] = Commutative[x] with Idempotent[x]
      override def Join: Join[Set[A]] = Associative.SetIdempotentInverse
      override def Meet: Meet[Set[A]] = new Commutative[Set[A]] with Idempotent[Set[A]] {
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
