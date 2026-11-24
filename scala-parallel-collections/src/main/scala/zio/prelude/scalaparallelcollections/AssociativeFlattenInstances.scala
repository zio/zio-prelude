package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.{immutable => par}

/** `AssociativeFlatten` and `IdentityFlatten` instances for `ParMap` and `ParSeq`. */
trait AssociativeFlattenInstances {

  /**
   * The `AssociativeFlatten` instance for `ParMap`. Flattening merges the inner
   * maps into one map and drops the outer keys. When inner maps share a key,
   * the result keeps one of their values.
   */
  implicit def ParMapAssociativeFlatten[K]: AssociativeFlatten[({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new AssociativeFlatten[({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def flatten[V](ffa: par.ParMap[K, par.ParMap[K, V]]): par.ParMap[K, V] =
        ffa.aggregate(par.ParMap.empty[K, V])({ case (l, (_, r)) => l ++ r }, _ ++ _)
    }

  /**
   * The `IdentityFlatten` (and thus `AssociativeFlatten`) instance for `ParSeq`.
   * Flattening concatenates the inner sequences in order, and `any` is
   * `ParSeq(())`.
   */
  implicit lazy val ParSeqIdentityFlatten: IdentityFlatten[par.ParSeq] =
    new IdentityFlatten[par.ParSeq] {
      def any: par.ParSeq[Any] = par.ParSeq(())

      def flatten[A](ffa: par.ParSeq[par.ParSeq[A]]): par.ParSeq[A] =
        ffa.fold(par.ParSeq[A]())(_ ++ _)
    }

}
