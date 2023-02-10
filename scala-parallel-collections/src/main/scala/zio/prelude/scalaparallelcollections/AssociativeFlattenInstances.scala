package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.{immutable => par}

trait AssociativeFlattenInstances {

  /**
   * The `AssociativeFlatten` instance for `ParMap`
   */
  implicit def ParMapAssociativeFlatten[K]: AssociativeFlatten[ParMap[K, +*]] =
    new AssociativeFlatten[ParMap[K, +*]] {
      def flatten[V](ffa: par.ParMap[K, par.ParMap[K, V]]): par.ParMap[K, V] =
        ffa.aggregate(par.ParMap.empty[K, V])({ case (l, (_, r)) => l ++ r }, _ ++ _)
    }

  /**
   * The `IdentityFlatten` (and thus `AssociativeFlatten`) instance for `ParSeq`.
   */
  implicit lazy val ParSeqIdentityFlatten: IdentityFlatten[par.ParSeq] =
    new IdentityFlatten[par.ParSeq] {
      def any: par.ParSeq[Any] = par.ParSeq(())

      def flatten[A](ffa: par.ParSeq[par.ParSeq[A]]): par.ParSeq[A] =
        ffa.fold(par.ParSeq[A]())(_ ++ _)
    }

}
