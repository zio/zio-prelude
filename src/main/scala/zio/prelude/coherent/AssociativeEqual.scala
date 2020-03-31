package zio.prelude.coherent

import zio.prelude._

trait AssociativeEqual {

  /**
   * Derives a `Associative[A] with Equal[A]` given a Associative[A]` and an `Equal[A]`.
   */
  implicit def associativeEqual[A](
    implicit associative0: Associative[A],
    equal0: Equal[A]
  ): Associative[A] with Equal[A] =
    new Associative[A] with Equal[A] {
      override def combine(l: => A, r: => A): A =
        associative0.combine(l, r)
      override def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
