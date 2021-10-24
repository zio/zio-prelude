package zio.prelude
package experimental

import zio.prelude.newtypes.{AndF, OrF}

trait Absorption[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]

}

object Absorption {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Absorption[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Absorption[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    absorption: Absorption.Aux[A, Join, Meet]
  ): Absorption.Aux[A, Join, Meet] =
    absorption

  implicit lazy val BoolInstance: DistributiveAbsorption[Boolean] with ExcludedMiddle[Boolean] with Involution[
    Boolean
  ] with Noncontradiction[Boolean] {
    type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
    type Meet[x] = Commutative[x] with Idempotent[x] with Inverse[x]
  } =
    new DistributiveAbsorption[Boolean]
      with ExcludedMiddle[Boolean]
      with Involution[Boolean]
      with Noncontradiction[Boolean] {

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

  implicit def SetInstance[A]: DistributiveAbsorption[Set[A]] {
    type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
    type Meet[x] = Commutative[x] with Idempotent[x]
  } =
    new DistributiveAbsorption[Set[A]] {
      override type Join[x] = Commutative[x] with Idempotent[x] with Inverse[x]
      override type Meet[x] = Commutative[x] with Idempotent[x]

      override def join(l: => Set[A], r: => Set[A]): Set[A] = l | r
      override def meet(l: => Set[A], r: => Set[A]): Set[A] = l & r

      override def Join: Join[OrF[Set[A]]] =
        new Commutative[OrF[Set[A]]] with Idempotent[OrF[Set[A]]] with Inverse[OrF[Set[A]]] {
          def combine(l: => OrF[Set[A]], r: => OrF[Set[A]]): OrF[Set[A]] = OrF((l: Set[A]) | (r: Set[A]))
          val identity: OrF[Set[A]]                                      = OrF(Set.empty)
          def inverse(l: => OrF[Set[A]], r: => OrF[Set[A]]): OrF[Set[A]] = OrF((l: Set[A]) &~ (r: Set[A]))
        }

      override def Meet: Meet[AndF[Set[A]]] =
        new Commutative[AndF[Set[A]]] with Idempotent[AndF[Set[A]]] {
          def combine(l: => AndF[Set[A]], r: => AndF[Set[A]]): AndF[Set[A]] = AndF((l: Set[A]) & (r: Set[A]))
        }
    }
}

trait AbsorptionSyntax {

  /**
   * Provides infix syntax for joining or meeting two values.
   */
  implicit class AbsorptionOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit absorption: Absorption.Aux[A, Associative, Associative]): A =
      absorption.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit absorption: Absorption.Aux[A, Associative, Associative]): A =
      absorption.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit absorption: Absorption.Aux[A, Associative, Associative]): A =
      absorption.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit absorption: Absorption.Aux[A, Associative, Associative]): A =
      absorption.meet(l, r)
  }

}
