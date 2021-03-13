package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.{immutable => par}

trait InvariantInstances {

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `ParMap`.
   */
  implicit def ParMapForEach[K]: ForEach[({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new ForEach[({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def forEach[G[+_]: IdentityBoth: Covariant, V, V2](map: par.ParMap[K, V])(f: V => G[V2]): G[par.ParMap[K, V2]] =
        map.aggregate[G[par.ParMap[K, V2]]](par.ParMap.empty[K, V2].succeed)(
          { case (m, (k, v)) => m.zipWith(f(v))((m, v) => m + (k -> v)) },
          _.zipWith(_)(_ ++ _)
        )
    }

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `ParSeq`.
   */
  implicit lazy val ParSeqForEach: ForEach[par.ParSeq] = new ForEach[par.ParSeq] {
    def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: par.ParSeq[A])(f: A => G[B]): G[par.ParSeq[B]] =
      fa.aggregate[G[par.ParSeq[B]]](par.ParSeq.empty.succeed)(
        (bs, a) => bs.zipWith(f(a))(_ :+ _),
        _.zipWith(_)(_ ++ _)
      )
  }

  /** The `Invariant` instance for `ParSet` */
  implicit lazy val ParSetInvariant: Invariant[par.ParSet] = new Invariant[par.ParSet] {
    def invmap[A, B](f: A <=> B): par.ParSet[A] <=> par.ParSet[B] =
      Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
  }
}
