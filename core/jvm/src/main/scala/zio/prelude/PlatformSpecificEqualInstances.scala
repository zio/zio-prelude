package zio.prelude

import zio.prelude.Equal.make
import zio.prelude.coherent.HashPartialOrd

import scala.collection.parallel.{immutable => par}

trait PlatformSpecificEqualInstances {

  /**
   * Derives an `PartialOrd[ParMap[A, B]]` (and thus `Equal[ParMap[A, B]]`) given an `Equal[B]`.
   * Due to the limitations of Scala's `ParMap`, this uses object equality on the keys.
   */
  implicit def ParMapPartialOrd[A, B: Equal]: PartialOrd[par.ParMap[A, B]] = new PartialOrd[par.ParMap[A, B]] {
    protected def checkCompare(l: par.ParMap[A, B], r: par.ParMap[A, B]): PartialOrdering = l.compareStrict(r)

    override protected def checkEqual(l: par.ParMap[A, B], r: par.ParMap[A, B]): Boolean =
      l.size === r.size &&
        l.forall { case (key, value) => r.get(key).fold(false)(_ === value) }
  }

  /**
   * Derives an `Equal[ParSeq[A]]` given an `Equal[A]`.
   */
  implicit def ParSeqEqual[A: Equal]: Equal[par.ParSeq[A]] =
    make((l, r) => l.length === r.length && l.corresponds(r)(_ === _))

  /**
   * `PartialOrd` and `Hash` (and thus also `Equal`) instance for `ParSet[A]` values.
   * Due to the limitations of Scala's `ParSet`,
   * this uses object equality and hash code on the elements.
   */
  implicit def ParSetHashPartialOrd[A]: Hash[par.ParSet[A]] with PartialOrd[par.ParSet[A]] =
    HashPartialOrd.make(
      _.hashCode,
      (l, r) =>
        if (l == r) Ordering.Equals
        else if (l.subsetOf(r)) Ordering.LessThan
        else if (r.subsetOf(l)) Ordering.GreaterThan
        else PartialOrdering.Incomparable,
      _ == _
    )

}
