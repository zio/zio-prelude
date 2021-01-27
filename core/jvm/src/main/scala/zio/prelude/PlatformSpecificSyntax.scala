package zio.prelude

import scala.collection.parallel.immutable._

trait PlatformSpecificSyntax {

  implicit class ParMapOps[K, V](private val l: ParMap[K, V]) {

    /** Compares two maps, where you supply `compareValues` that compares the common values */
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
