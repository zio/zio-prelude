package zio.prelude

import scala.collection.parallel.{immutable => par}

trait PlatformSpecificInvariantInstances {

  /**
   * The `Covariant` (and thus `Invariant`) instance for `ParMap`.
   */
  implicit def ParMapCovariant[K]: Covariant[({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new Covariant[({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def map[A, B](f: A => B): par.ParMap[K, A] => par.ParMap[K, B] = { m =>
        // mapValues returns `parallel.ParMap`, not `parallel.immutable.ParMap`
        m.map { case (k, v) => (k, f(v)) }
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `ParSeq`.
   */
  implicit lazy val ParSeqCovariant: Covariant[par.ParSeq] =
    new Covariant[par.ParSeq] {
      def map[A, B](f: A => B): par.ParSeq[A] => par.ParSeq[B] = { s =>
        s.map(f)
      }
    }

  /** The `Invariant` instance for `ParSet` */
  implicit lazy val ParSetInvariant: Invariant[par.ParSet] =
    new Invariant[par.ParSet] {
      def invmap[A, B](f: A <=> B): par.ParSet[A] <=> par.ParSet[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }
}
