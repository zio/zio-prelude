package zio.prelude
package scalaparallelcollections

import zio.prelude.PartialOrd.makeFrom

import scala.annotation.tailrec
import scala.collection.parallel.{immutable => par}

/** The `PartialOrd` instance for `ParSeq`. */
trait PartialOrdInstances {

  /**
   * Derives a `PartialOrd[ParSeq[A]]` given a `PartialOrd[A]`. Sequences
   * compare element by element and the first pair of elements that are not
   * equal decides the result, `Incomparable` included; when one sequence is a
   * prefix of the other, the shorter one is `LessThan`.
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
      ParSeqEqual
    )

}
