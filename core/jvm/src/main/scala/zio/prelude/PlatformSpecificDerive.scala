package zio.prelude

import scala.collection.parallel.{immutable => par}

trait PlatformSpecificDerive {

  /**
   * The `DeriveEqual` instance for `ParMap`.
   */
  implicit def ParMapDeriveEqual[A]: DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = par.ParMap[A, x] })#lambda] {
      def derive[B: Equal]: Equal[par.ParMap[A, B]] =
        Equal.ParMapPartialOrd
    }

  /**
   * The `DeriveEqual` instance for `ParSeq`.
   */
  // to avoid conflict with `zio.prelude.ParSeq`
  implicit lazy val parParSeqDeriveEqual: Derive[par.ParSeq, Equal] =
    new Derive[par.ParSeq, Equal] {
      def derive[A: Equal]: Equal[par.ParSeq[A]] =
        Equal.ParSeqEqual
    }

  /**
   * The `DeriveEqual` instance for `ParSet`.
   */
  implicit def ParSetDeriveEqual[A]: DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] =
    new DeriveEqual[({ type lambda[x] = par.ParSet[A] })#lambda] {
      def derive[B: Equal]: Equal[par.ParSet[A]] =
        Equal.ParSetHashPartialOrd
    }
}
