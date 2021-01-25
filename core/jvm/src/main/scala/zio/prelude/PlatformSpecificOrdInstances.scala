package zio.prelude

import zio.prelude.Ord.makeFrom

import scala.annotation.tailrec
import scala.collection.parallel.{immutable => par}

trait PlatformSpecificOrdInstances {

  /**
   * Derives an `Ord[ParSeq[A]]` given an `Ord[A]`.
   */
  implicit def ParSeqOrd[A: Ord]: Ord[par.ParSeq[A]] =
    makeFrom(
      { (l, r) =>
        val j    = l.length
        val k    = r.length
        val OrdA = Ord[A]

        @tailrec
        def loop(i: Int): Ordering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else {
            val compare = OrdA.compare(l(i), r(i))
            if (compare.isEqual) loop(i + 1) else compare
          }

        loop(0)
      },
      PartialOrd.ParSeqPartialOrd
    )
}
