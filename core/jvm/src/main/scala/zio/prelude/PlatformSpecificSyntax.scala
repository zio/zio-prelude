package zio.prelude

import collection.parallel.immutable._

trait PlatformSpecificSyntax {

  /**
   * Provides platform specific syntax for combining values
   * in a `ParIterable` which is thus done in a parallel manner.
   */
  implicit class ParIterableOps[A](
    private val p: ParIterable[A]
  ) {

    /**
     * Commutatively combines the values in a parallel manner,
     * while blocking the thread.
     */
    def reduceCommutative(implicit commutative: Commutative[A]): Option[A] =
      p.reduceOption(commutative.combine(_, _))

    /**
     * Returns an effect, that commutatively combines the values in a parallel manner,
     * while ensuring the current thread isn't blocked.
     */
    def reduceCommutativeM(implicit commutative: Commutative[A]): zio.RIO[zio.blocking.Blocking, Option[A]] =
      zio.blocking.effectBlocking(reduceCommutative)

    /**
     * Commutatively combines the values in a parallel manner, returning the `identity` element if empty,
     * while blocking the thread.
     */
    def reduceCommutativeIdentity(implicit commutative: Commutative[A], identity: Identity[A]): A =
      p.fold(identity.identity)(commutative.combine(_, _))

    /**
     * Returns an effect, that commutatively combines the values in a parallel manner, returning the `identity` element if empty,
     * while ensuring the current thread isn't blocked.
     */
    def reduceCommutativeIdentityM(implicit
      commutative: Commutative[A],
      identity: Identity[A]
    ): zio.RIO[zio.blocking.Blocking, A] =
      zio.blocking.effectBlocking(reduceCommutativeIdentity)
  }

  implicit class ParMapOps[K, V](private val l: ParMap[K, V]) {

    /** Compares two maps, where you supply `compareValues` that compares the common values */
    def compareWith(
      compareValues: (Ordering, ParIterable[(V, V)]) => PartialOrdering
    )(r: ParMap[K, V]): PartialOrdering = {
      def commonValues(lesserMap: ParMap[K, V]): ParIterable[(V, V)] =
        lesserMap.map[(V, V), ParIterable[(V, V)]] { case (k, _) => (l(k), r(k)) }
      if (l.keySet == r.keySet) {
        compareValues(Ordering.Equals, commonValues(l))
      } else if (l.keySet.subsetOf(r.keySet)) {
        compareValues(Ordering.LessThan, commonValues(l))
      } else if (r.keySet.subsetOf(l.keySet)) {
        compareValues(Ordering.GreaterThan, commonValues(r))
      } else {
        PartialOrdering.Incomparable
      }
    }

    /** Compares two maps, allowing for the values to be lesser in the lesser map or greater in the greater map */
    def compareSoft(r: ParMap[K, V])(implicit V: PartialOrd[V]): PartialOrdering = {
      def compareValues(expected: Ordering, commonValues: ParIterable[(V, V)]): PartialOrdering =
        commonValues.map { case (l, r) => l =??= r }.fold(expected)(_.unify(_))
      compareWith(compareValues)(r)
    }

    /** Compares two maps, expecting the values for the common keys to be equal. */
    def compareStrict(r: ParMap[K, V])(implicit V: Equal[V]): PartialOrdering = {
      def compareValues(expected: Ordering, commonValues: ParIterable[(V, V)]): PartialOrdering =
        if (commonValues.forall { case (l, r) => l === r }) {
          expected
        } else {
          PartialOrdering.Incomparable
        }
      compareWith(compareValues)(r)
    }
  }

}
