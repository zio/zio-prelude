package zio.prelude.coherent

import zio.prelude._

trait HashCoherent {

  /**
   * Derive a `Hash[A] with Ord[A]` given a `Hash[A]` and an `Ord[A]`.
   */
  implicit def hashOrd[A](implicit hash0: Hash[A], ord0: Ord[A]): Hash[A] with Ord[A] =
    new Hash[A] with Ord[A] {
      def checkCompare(l: A, r: A): Ordering =
        ord0.compare(l, r)
      def hash(a: A): Int =
        hash0.hash(a)
      override def checkEqual(l: A, r: A): Boolean =
        ord0.equal(l, r)
      override def contramap[B](f: B => A): Hash[B] with Ord[B] =
        hashOrd(hash0.contramap(f), ord0.contramap(f))
    }
}
