package zio.prelude.coherent

import zio.prelude._

trait ClosureEqual {

  /**
   * Derives a `Closure[A] with Equal[A]` given a closure[A]` and an `Equal[A]`.
   */
  implicit def closureEqual[A](implicit closure0: Closure[A], equal0: Equal[A]): Closure[A] with Equal[A] =
    new Closure[A] with Equal[A] {
      override def combine(l: => A, r: => A): A =
        closure0.combine(l, r)

      override def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
