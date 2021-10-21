package zio.prelude
package scalaparallelcollections

import zio.prelude.Hash.makeFrom

import scala.collection.parallel.{immutable => par}

trait HashInstances {

  /**
   * Derives a `Hash[ParMap[A, B]]` given a `Hash[B]`. Due to the limitations of
   * Scala's `ParMap`, this uses object equality and hash code on the keys.
   */
  implicit def ParMapHash[A, B: Hash]: Hash[par.ParMap[A, B]] =
    makeFrom(_.map { case (k, v) => (k, v.hash) }.hashCode, ParMapPartialOrd)

  /**
   * Derives a `Hash[ParSeq[A]]` given a `Hash[A]`.
   */
  implicit def ParSeqHash[A: Hash]: Hash[par.ParSeq[A]] =
    makeFrom(_.map(_.hash).hashCode, ParSeqEqual)

}
