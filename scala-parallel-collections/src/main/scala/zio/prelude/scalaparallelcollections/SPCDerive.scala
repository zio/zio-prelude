package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.{immutable => par}

/** `DeriveEqual` instances for `ParMap`, `ParSeq` and `ParSet`. */
trait SPCDerive {

  /**
   * The `DeriveEqual` instance for `ParMap`.
   */
  implicit def ParMapDeriveEqual[A]: DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] {
      def derive[B: Equal]: Equal[par.ParMap[A, B]] =
        ParMapPartialOrd
    }

  // to avoid conflict with `zio.prelude.ParSeq`
  /**
   * The `DeriveEqual` instance for `ParSeq`.
   */
  implicit lazy val parParSeqDeriveEqual: Derive[par.ParSeq, Equal] =
    new Derive[par.ParSeq, Equal] {
      def derive[A: Equal]: Equal[par.ParSeq[A]] =
        ParSeqEqual
    }

  /**
   * The `DeriveEqual` instance for `ParSet[A]`, seen as a type constructor that
   * ignores its parameter. `derive` returns the `ParSet` instance that uses
   * object equality on the elements; it does not use the given `Equal`.
   */
  implicit def ParSetDeriveEqual[A]: DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] =
    new DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] {
      def derive[B: Equal]: Equal[par.ParSet[A]] =
        ParSetHashPartialOrd
    }
}
