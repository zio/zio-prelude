/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.prelude.newtypes.{Max, Natural, Prod, Sum}

import scala.language.implicitConversions

/**
 * Similar to `ZSet`, a `ZNonEmptySet[A, B]` is a guaranteed non-empty set of `A` values where `B` represents some notion of
 * "how many" `A` values are included in the set. This can be the number of
 * times each element appears in the set if `B` is a natural number, the
 * probability associated with an element in the set if `B` is a rational
 * number, or even whether an element appears at all if `B` is a boolean.
 */
final class ZNonEmptySet[+A, +B] private (private val zset: ZSet[A, B]) { self =>

  /**
   * A symbolic alias for `zip`.
   */
  def <*>[B1 >: B, C](
    that: ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[(A, C), B1] =
    self zip that

  /**
   * A symbolic alias for `combine`.
   */
  def <>[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[A1, B1] =
    self combine that

  /**
   * A symbolic alias for `union`.
   */
  def |[A1 >: A, B1 >: B](
    that: ZSet[A1, B1]
  )(implicit ev1: Commutative[Max[B1]], ev2: Identity[Sum[B1]]): ZNonEmptySet[A1, B1] =
    self union that

  /**
   * Returns the number of times the specified element appears in the set.
   */
  def apply[A1 >: A, B1 >: B](a: A1)(implicit ev: Identity[Sum[B1]]): B1 =
    zset[A1, B1](a)(ev)

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the sum of the number of times it
   * appears in this set and the specified set.
   */
  def combine[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[A1, B1] =
    new ZNonEmptySet(zset.combine(that))

  /**
   * Returns whether this set is equal to the specified set, meaning that the
   * same elements appear in both sets the same number of times.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: ZNonEmptySet[_, _]      => self.zset == that.toZSet
      case _                             => false
    }

  /**
   * Creates a new set for each element in this set and combines the resulting
   * sets together. The number of times each element appears will be the sum
   * of the products of the number of times it appeared in the original set
   * and the number of times it appears in each new set.
   */
  def flatMap[B1 >: B, C](
    f: A => ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[C, B1] =
    new ZNonEmptySet[C, B1](zset.flatMap(f(_).toZSet))

  /**
   * Returns the hash code of this set.
   */
  override def hashCode: Int =
    zset.hashCode

  /**
   * Transforms the elements in the set using the specified function. If this
   * results in mapping two or more elements to the same values, the number of
   * times the new value appears in the set will be the sum of the number of
   * times each of the old values appeared in the set.
   */
  def map[B1 >: B, C](f: A => C)(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[C, B1] =
    new ZNonEmptySet(zset.map[B1, C](f)(ev))

  /**
   * Transforms the representation of how many times each element appears in
   * the set with the specified function.
   */
  def transform[C](f: B => C): ZNonEmptySet[A, C] =
    new ZNonEmptySet(zset.transform(f))

  /**
   * Converts this set to a `Map` from elements to how many times they appear
   * in the set.
   */
  def toMap[A1 >: A]: Map[A1, B] =
    zset.toMap

  /**
   * Converts this set to a `Set`, discarding information about how many times
   * an element appears in the set beyond whether it appears at all.
   */
  def toNonEmptySet[A1 >: A, B1 >: B](implicit ev1: Equal[B1], ev2: Identity[Sum[B1]]): NonEmptySet[A1] =
    toMap.tail.foldLeft(NonEmptySet.single[A1](toMap.head._1)) { case (set, (a, b)) =>
      if (ev1.notEqual(b, ev2.identity)) set + a else set
    }

  /**
   * Returns a meaningful string representation of this set.
   */
  override def toString: String =
    toMap.mkString("ZNonEmptySet(", ", ", ")")

  def toZSet: ZSet[A, B] = zset

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the maximum of the number of times
   * it appears in this set and the specified set.
   */
  def union[A1 >: A, B1 >: B](
    that: ZSet[A1, B1]
  )(implicit ev1: Commutative[Max[B1]], ev2: Identity[Sum[B1]]): ZNonEmptySet[A1, B1] =
    new ZNonEmptySet(zset.union(that))

  /**
   * Combines this set with the specified set to produce their cartesian
   * product.
   */
  def zip[B1 >: B, C](
    that: ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[(A, C), B1] =
    zipWith(that)((_, _))

  /**
   * Combines this set with the specified set to produce their cartesian
   * product, combining pair of elements using the specified function `f`.
   */
  def zipWith[B1 >: B, C, D](
    that: ZNonEmptySet[C, B1]
  )(f: (A, C) => D)(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[D, B1] =
    new ZNonEmptySet(zset.zipWith(that)(f))
}

object ZNonEmptySet extends LowPriorityZNonEmptySetImplicits {

  /**
   * Constructs a set with the specified elements.
   */
  def apply[A](a: A, as: A*): NonEmptyMultiSet[A] =
    fromIterable(a, as)

  /**
   * Constructs a set from the specified `Iterable`. The measure of how many
   * times a value occurs in the set will be an integer representing how many
   * times the value occurred in the specified `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptyMultiSet[A] =
    new ZNonEmptySet(ZSet.fromIterable(Iterable(head) ++ tail))

  /**
   * Constructs a set from the specified `Iterable`. The measure of how many
   * times a value occurs in the set will be an integer representing how many
   * times the value occurred in the specified `Iterable`. Returns `None` if empty.
   */
  def fromIterableOption[A](elems: Iterable[A]): Option[NonEmptyMultiSet[A]] =
    if (elems.isEmpty) None
    else Some(new ZNonEmptySet(ZSet.fromIterable(elems)))

  /**
   * Constructs a set from the specified `Set`. The measure of how many times
   * a value occurs in the set will be a boolean representing whether a value
   * occurs at all.
   */
  def fromNonEmptySet[A](set: NonEmptySet[A]): ZNonEmptySet[A, Boolean] =
    new ZNonEmptySet(ZSet.fromSet(set.toSet))

  /**
   * Constructs a set from the specified `Map`. The values will be the keys in
   * the `Map` and the measure of how many times a value occurs will be the
   * keys value. Returns `None` if empty.
   */
  def fromMapOption[A, B](map: Map[A, B]): Option[ZNonEmptySet[A, B]] =
    if (map.isEmpty) None
    else Some(new ZNonEmptySet(ZSet.fromMap(map)))

  /**
   * Constructs a `NonEmptyMultiSet`, where, by definition, each element is present exactly once.
   * Returns `None` if empty.
   */
  def fromSetOption[A](set: Set[A]): Option[ZNonEmptySet[A, Boolean]] =
    if (set.isEmpty) None
    else Some(new ZNonEmptySet(ZSet.fromSet(set)))

  /**
   * Derives a `Associative[ZNonEmptySet[A, B]]` given a `Associative[B]`.
   */
  implicit def ZNonEmptySetAssociative[A, B](implicit ev: Associative[B]): Associative[ZNonEmptySet[A, B]] =
    new Idempotent[ZNonEmptySet[A, B]] {
      override def combine(l: => ZNonEmptySet[A, B], r: => ZNonEmptySet[A, B]): ZNonEmptySet[A, B] =
        new ZNonEmptySet(ZSet.ZSetIdentity(ev).combine(l, r))
    }

  /**
   * Derives a `Commutative[ZNonEmptySet[A, B]]` given a `Commutative[B]`.
   */
  implicit def ZNonEmptySetCommutative[A, B](implicit ev: Commutative[B]): Commutative[ZNonEmptySet[A, B]] =
    new Commutative[ZNonEmptySet[A, B]] {
      def combine(left: => ZNonEmptySet[A, B], right: => ZNonEmptySet[A, B]): ZNonEmptySet[A, B] =
        new ZNonEmptySet(ZSet.ZSetCommutative(ev).combine(left, right))
    }

  /**
   * Derives a `Debug[ZNonEmptySet[A, B]]` given a `Debug[A]` and `Debug[B]`.
   */
  implicit def ZNonEmptySetDebug[A: Debug, B: Debug]: Debug[ZNonEmptySet[A, B]] =
    chunk =>
      Debug.Repr.VConstructor(List("zio", "prelude"), "ZNonEmptySet", chunk.toMap.toList.map((t: (A, B)) => t.debug))

  /**
   * Derives an `Equal[ZNonEmptySet[A, B]]` given an `Equal[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZNonEmptySetEqual[A, B: Equal](implicit ev: Identity[Sum[B]]): Equal[ZNonEmptySet[A, B]] =
    Equal[ZSet[A, B]].contramap(_.toZSet)

  /**
   * The `EqualF` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetDeriveEqual[B: Equal](implicit
    ev: Identity[Sum[B]]
  ): DeriveEqual[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def derive[A: Equal]: Equal[ZNonEmptySet[A, B]] =
        ZNonEmptySetEqual
    }

  /**
   * The `Covariant` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetCovariant[B](implicit
    ev: Commutative[Sum[B]]
  ): Covariant[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new Covariant[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def map[A, C](f: A => C): ZNonEmptySet[A, B] => ZNonEmptySet[C, B] =
        _.map(f)
    }

  /**
   * The `IdentityFlatten` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetFlatten[B](implicit
    ev1: Commutative[Sum[B]],
    ev2: Commutative[Prod[B]]
  ): AssociativeFlatten[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new AssociativeFlatten[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def flatten[A](ffa: ZNonEmptySet[ZNonEmptySet[A, B], B]): ZNonEmptySet[A, B] =
        ffa.flatMap(identity)
    }

  /**
   * Derives a `Idempotent[ZNonEmptySet[A, B]]` given a `Idempotent[B]`.
   */
  implicit def ZNonEmptySetIdempotent[A, B](implicit ev: Idempotent[B]): Idempotent[ZNonEmptySet[A, B]] =
    new Idempotent[ZNonEmptySet[A, B]] {
      override def combine(l: => ZNonEmptySet[A, B], r: => ZNonEmptySet[A, B]): ZNonEmptySet[A, B] =
        new ZNonEmptySet(ZSet.ZSetIdempotent(ev).combine(l, r))
    }

  /**
   * Derives a `Hash[ZNonEmptySet[A, B]]` given a `Hash[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZNonEmptySetHash[A, B: Hash](implicit ev: Identity[Sum[B]]): Hash[ZNonEmptySet[A, B]] =
    Hash[ZSet[A, B]].contramap(_.zset)

  /**
   * Provides an implicit conversion from `NonEmptySet` to the `Set`
   * for interoperability with Scala's collection library.
   */
  implicit def toZSet[A, B](zNonEmptySet: ZNonEmptySet[A, B]): ZSet[A, B] =
    zNonEmptySet.toZSet
}

trait LowPriorityZNonEmptySetImplicits {

  /**
   * Derives a `PartialOrd[ZNonEmptySet[A, B]]` given a `PartialOrd[B]`.
   * Due to the limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZNonEmptySetPartialOrd[A, B: PartialOrd](implicit ev: Identity[Sum[B]]): PartialOrd[ZNonEmptySet[A, B]] =
    PartialOrd[ZSet[A, B]].contramap(_.toZSet)

}

trait ZNonEmptySetSyntax {
  implicit final class ZNonEmptySetMapOps[+A](self: Map[A, Natural]) {

    /** Returns a `NonEmptyMultiSet` or `None` if the original Multiset is empty */
    def toNonEmptyMultiSetOption: Option[NonEmptyMultiSet[A]] = NonEmptyMultiSet.fromMapOption(self)
  }

  implicit final class ZNonEmptySetNonEmptyMultiSetOps[+A](self: NonEmptyMultiSet[A]) {

    /** Returns an element */
    def head: A = peel._1

    /**
     * Returns an element of this `NonEmptyMultiSet` and the remainder, which is a (possibly empty) `MultiSet`.
     */
    def peel: (A, MultiSet[A]) =
      self.toZSet.peel.get

    /**
     * Returns an element of this `NonEmptyMultiSet`
     * and the remainder or `None`, if the remainder is empty.
     */
    def peelNonEmpty: (A, Option[NonEmptyMultiSet[A]]) = {
      val (head, tail) = peel
      (head, tail.toNonEmptyZSet)
    }

    /**
     * Returns the tail of this `NonEmptyMultiSet` as a (possibly empty) `MultiSet`.
     */
    def tail: MultiSet[A] =
      peel._2

    /**
     * Returns the tail of this `NonEmptyMultiSet` if it exists or `None` otherwise.
     */
    def tailNonEmpty: Option[NonEmptyMultiSet[A]] =
      peelNonEmpty._2
  }
}
