package zio.prelude.coherent

import zio.prelude._

trait CommutativeCoherent extends CommutativeCoherentLowPriority {

  /**
   * Derives a `Commutative[A] with Equal[A]` given a `Commutative[A]` and an `Equal[A]`.
   */
  implicit def associativeCommutativeEqual[A](
    implicit associative0: Associative[A],
    commutative0: Commutative[A],
    equal0: Equal[A]
  ): Associative[A] with Commutative[A] with Equal[A] =
    new Associative[A] with Commutative[A] with Equal[A] {

      val _ = associative0

      override def combine(l: => A, r: => A): A = commutative0.combine(l, r)

      override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait CommutativeCoherentLowPriority {

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
