package zio.prelude.coherent

import zio.prelude._

trait CommutativeEqual {

  /**
   * Derives a `Commutative[A] with Equal[A]` given a `Commutative[A]` and an `Equal[A]`.
   */
  implicit def commutativeEqual[A](
    implicit commutative0: Commutative[A],
    equal0: Equal[A]
  ): Commutative[A] with Equal[A] =
    new Commutative[A] with Equal[A] {
      override def combine(l: => A, r: => A): A = commutative0.combine(l, r)

      override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
