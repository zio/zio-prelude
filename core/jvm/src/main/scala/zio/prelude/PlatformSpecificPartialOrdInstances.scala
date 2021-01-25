package zio.prelude

import zio.prelude.PartialOrd.makeFrom

import scala.annotation.tailrec
import scala.collection.parallel.{immutable => par}

trait PlatformSpecificPartialOrdInstances {

  /**
   * Derives an `PartialOrd[ParMap[A, B]]` given an `Equal[B]`.
   * Due to the limitations of Scala's `ParMap`, this uses object equality on the keys.
   */
  implicit def ParMapPartialOrd[A, B: Equal]: PartialOrd[par.ParMap[A, B]] =
    PartialOrd.makeFrom(_.compareStrict(_), Equal.ParMapEqual)

  /**
   * Derives an `PartialOrd[ParSeq[A]]` given an `PartialOrd[A]`.
   */
  implicit def ParSeqPartialOrd[A: PartialOrd]: PartialOrd[par.ParSeq[A]] =
    makeFrom(
      { (l, r) =>
        val j           = l.length
        val k           = r.length
        val PartialOrdA = PartialOrd[A]

        @tailrec
        def loop(i: Int): PartialOrdering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else
            PartialOrdA.compare(l(i), r(i)) match {
              case Ordering.Equals => loop(i + 1)
              case compare         => compare
            }

        loop(0)
      },
      Equal.ParSeqEqual
    )
}
