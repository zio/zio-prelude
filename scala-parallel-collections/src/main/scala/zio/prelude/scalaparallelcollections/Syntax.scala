package zio.prelude
package scalaparallelcollections

import scala.collection.parallel.immutable._

/** Comparison syntax for `ParMap`. */
trait Syntax {

  /** Adds comparison methods to a `ParMap`. Keys are compared with object equality. */
  implicit class ParMapOps[K, V](private val l: ParMap[K, V]) {

    /**
     * Compares two maps, where you supply `compareValues` that compares the common values.
     *
     * When the key sets are equal, `compareValues` receives `Ordering.Equals`;
     * when the keys of this map are a proper subset of the keys of `r`, it
     * receives `Ordering.LessThan`; in the opposite case, `Ordering.GreaterThan`.
     * It also receives the pairs of values (this map's value, `r`'s value) for
     * the keys of the smaller map. When neither key set contains the other,
     * the result is `Incomparable` and `compareValues` is not called.
     */
    def compareWith(
      compareValues: (Ordering, ParIterable[(V, V)]) => PartialOrdering
    )(r: ParMap[K, V]): PartialOrdering = {
      def commonValues(lesserMap: ParMap[K, V]): ParIterable[(V, V)] =
        // `toIterable` so that we don't incidentally create a map (and thus drop duplicate would-be keys)
        lesserMap.toIterable.map { case (k, _) => (l(k), r(k)) }
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

    /**
     * Compares two maps, allowing for the values to be lesser in the lesser map or greater in the greater map.
     *
     * The key sets give the expected result, as in `compareWith`, and each pair
     * of common values must compare as equal or in that same direction;
     * otherwise the result is `Incomparable`. With equal key sets, the values
     * decide: `ParMap("a" -> 1)` compared with `ParMap("a" -> 2)` is `LessThan`.
     */
    def compareSoft(r: ParMap[K, V])(implicit V: PartialOrd[V]): PartialOrdering = {
      def compareValues(expected: Ordering, commonValues: ParIterable[(V, V)]): PartialOrdering =
        commonValues.aggregate[PartialOrdering](expected)({ case (acc, (l, r)) => acc.unify(l =??= r) }, _.unify(_))
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
