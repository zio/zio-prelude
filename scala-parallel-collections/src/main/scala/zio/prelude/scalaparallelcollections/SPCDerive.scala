package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.{immutable => par}

trait SPCDerive {

  /**
   * The `DeriveEqual` instance for `ParMap`.
   */
  implicit def ParMapDeriveEqual[A]: DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] {
      def derive[B: Equal]: Equal[par.ParMap[A, B]] =
        ParMapPartialOrd
    }

  /**
   * The `DeriveEqual` instance for `ParSeq`.
   */
  // to avoid conflict with `zio.prelude.ParSeq`
  implicit val parParSeqDeriveEqual: Derive[par.ParSeq, Equal] =
    new Derive[par.ParSeq, Equal] {
      def derive[A: Equal]: Equal[par.ParSeq[A]] =
        ParSeqEqual
    }

  /**
   * The `DeriveEqual` instance for `ParSet`.
   */
  implicit def ParSetDeriveEqual[A]: DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] =
    new DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] {
      def derive[B: Equal]: Equal[par.ParSet[A]] =
        ParSetHashPartialOrd
    }
}
