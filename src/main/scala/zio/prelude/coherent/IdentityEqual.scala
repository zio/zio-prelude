package zio.prelude.coherent

import zio.prelude._

trait IdentityEqual {

  /**
   * Derives a `Identity[A] with Equal[A]` given a `Identity[A]` and an `Equal[A]`.
   */
  implicit def identityEqual[A](implicit identity0: Identity[A], equal0: Equal[A]): Identity[A] with Equal[A] =
    new Identity[A] with Equal[A] {
      def identity: A = identity0.identity

      def combine(l: A, r: A): A = identity0.combine(l, r)

      def equal(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
