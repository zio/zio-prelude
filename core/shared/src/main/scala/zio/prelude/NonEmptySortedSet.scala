/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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

import zio.NonEmptyChunk
import zio.prelude.coherent.HashPartialOrd

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.language.implicitConversions
import scala.math.{Ordering => SOrdering}

final class NonEmptySortedSet[A] private (private val set: SortedSet[A]) { self =>

  /** Converts this `NonEmptySortedSet` to a `SortedSet`. */
  def toSet: SortedSet[A] = set

  /**
   * Returns an element of this `NonEmptySortedSet` and the remainder, which is a (possibly empty) `SortedSet`.
   */
  @inline
  def peel: (A, SortedSet[A]) = (set.head, set.tail)

  /**
   * Returns an element of this `NonEmptySortedSet`
   * and the remainder or `None`, if the remainder is empty.
   */
  def peelNonEmpty: (A, Option[NonEmptySortedSet[A]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(new NonEmptySortedSet(tail)))
  }

  /**
   * Converts this `NonEmptySortedSet` to a `NonEmptyChunk`.
   */
  def toNonEmptyChunk: NonEmptyChunk[A] = peel match { case (head, tail) => NonEmptyChunk.fromIterable(head, tail) }

  /**
   * Converts this `NonEmptySortedSet` to a `NonEmptyList`.
   */
  def toNonEmptyList: NonEmptyList[A] = peel match { case (head, tail) => NonEmptyList.fromIterable(head, tail) }

  /**
   * Creates a new `NonEmptySortedSet` with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new set that contains all elements of this set and that also
   *          contains `elem`.
   */
  def +(elem: A): NonEmptySortedSet[A] = new NonEmptySortedSet(set + elem)

  /**
   * Computes the union between of `NonEmptySortedSet` and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new `NonEmptySortedSet` consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: SortedSet[A]): NonEmptySortedSet[A] = new NonEmptySortedSet(set.union(that))

  /**
   * Creates a new `NonEmptySortedSet` by adding all elements contained in another collection to this `NonEmptySortedSet`, omitting duplicates.
   *
   * This method takes a collection of elements and adds all elements, omitting duplicates, into `NonEmptySortedSet`.
   *
   * Example:
   *  {{{
   *    scala> val a = NonEmptySortedSet(1, 2) ++ NonEmptySortedSet(2, "a")
   *    a: zio.prelude.NonEmptySortedSet[Any] = NonEmptySortedSet(1, 2, a)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptySortedSet` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[A]): NonEmptySortedSet[A] = new NonEmptySortedSet(set ++ elems)

  /** Adds the `elem` to this `NonEmptySortedSet`. Alias for `+`. */
  def add(elem: A): NonEmptySortedSet[A] = self + elem

  /** Removes the `elem` from this `NonEmptySortedSet`. Alias for `-`. */
  def remove(elem: A): SortedSet[A] = set - elem

  /**
   * removes the elem from  `NonEmptySortedSet`, returning Some(NonEmptySortedSet) if there's anything
   * left, otherwise None
   */
  def removeNonEmpty(elem: A): Option[NonEmptySortedSet[A]] = {
    val newSet = set - elem
    if (newSet.nonEmpty) Some(new NonEmptySortedSet(newSet)) else None
  }

  /**
   * Returns the tail of this `NonEmptySortedSet` if it exists or `None` otherwise.
   */
  def tailNonEmpty: Option[NonEmptySortedSet[A]] = peelNonEmpty._2

  def map[B](f: A => B)(implicit sOrdering: SOrdering[B]): NonEmptySortedSet[B] = new NonEmptySortedSet(set.map(f))

  override def hashCode: Int = set.hashCode ^ NonEmptySortedSet.NonEmptySortedSetSeed

  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySortedSet[_]    => self.set == that.toSet
      case _                             => false
    }

  override def toString: String = s"NonEmpty$set"
}

object NonEmptySortedSet {
  def apply[A](elem: A, others: Iterable[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] = {
    val treeSet = new TreeSet() + elem ++ others
    new NonEmptySortedSet(treeSet)
  }

  def apply[A](elem: A, others: A*)(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elem, others)

  def unapply[A](arg: NonEmptySortedSet[A]): Some[(A, SortedSet[A])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptyChunk` from a `NonEmptyList`.
   */
  def fromNonEmptyChunk[A](elems: NonEmptyChunk[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedSet` from a `NonEmptyList`.
   */
  def fromNonEmptyList[A](elems: NonEmptyList[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedSet` from an element and `SortedSet`.
   */
  def fromSet[A](elem: A, others: SortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elem, others)

  /**
   * Constructs a `NonEmptySortedSet` from a `SortedSet` or `None` otherwise.
   */
  def fromSetOption[A](set: SortedSet[A])(implicit ordering: SOrdering[A]): Option[NonEmptySortedSet[A]] =
    set.headOption.map(fromSet(_, set.tail))

  /**
   * Constructs a `NonEmptySortedSet` from an `Iterable` or `None` otherwise.
   */
  def fromIterableOption[A](iterable: Iterable[A])(implicit ordering: SOrdering[A]): Option[NonEmptySortedSet[A]] =
    iterable.headOption.fold(None: Option[NonEmptySortedSet[A]])(h => Some(apply(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySortedSet` with the specified single value.
   */
  def single[A](head: A)(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(head)

  /** Creates a `NonEmptySortedSet` containing elements from `l` and `r` */
  def union[A](l: NonEmptySortedSet[A], r: SortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] = {
    val (head, tail) = l.peel
    NonEmptySortedSet.fromSet(head, tail.union(r))
  }

  /** Creates a `NonEmptySortedSet` containing elements from `l` and `r` */
  def union[A](l: SortedSet[A], r: NonEmptySortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    union(r, l)

  /**
   * The `Commutative` and `Idempotent` (and thus `Associative`) instance for `NonEmptySortedSet`.
   */
  implicit def NonEmptySortedSetCommutativeIdempotent[A](implicit
    aOrd: SOrdering[A]
  ): Commutative[NonEmptySortedSet[A]] with Idempotent[NonEmptySortedSet[A]] =
    new Commutative[NonEmptySortedSet[A]] with Idempotent[NonEmptySortedSet[A]] {
      override def combine(l: => NonEmptySortedSet[A], r: => NonEmptySortedSet[A]): NonEmptySortedSet[A] =
        l union r.toSet
    }

  /**
   * Derives a `Debug[NonEmptySortedSet[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptySortedSetDebug[A: Debug]: Debug[NonEmptySortedSet[A]] =
    chunk =>
      Debug.Repr.VConstructor(List("zio", "prelude"), "NonEmptySortedSet", chunk.toNonEmptyList.map(_.debug).toCons)

  /**
   * The `DeriveEqual` instance for `NonEmptySortedSet`.
   */
  implicit val NonEmptySortedSetDeriveEqual: DeriveEqual[NonEmptySortedSet] =
    new DeriveEqual[NonEmptySortedSet] {
      def derive[A: Equal]: Equal[NonEmptySortedSet[A]] =
        NonEmptySortedSetHashPartialOrd
    }

  /**
   * Derives a `Hash[NonEmptySortedSet[A]]` and `PartialOrd[NonEmptySortedSet[A]]` (and thus `Equal[NonEmptyList[A]]`) instance.
   */
  implicit def NonEmptySortedSetHashPartialOrd[A]: Hash[NonEmptySortedSet[A]] with PartialOrd[NonEmptySortedSet[A]] =
    HashPartialOrd.derive[Set[A]].contramap(_.toSet)

  /**
   * Provides an implicit conversion from `NonEmptySortedSet` to the `Set`
   * for interoperability with Scala's collection library.
   */
  implicit def toSet[A](nonEmptySet: NonEmptySortedSet[A]): Set[A] =
    nonEmptySet.toSet

  private val NonEmptySortedSetSeed: Int = 1247120194

}

trait NonEmptySortedSetSyntax {
  implicit final class NonEmptySortedSetIterableOps[A](private val iterable: Iterable[A])(implicit aOrd: SOrdering[A]) {

    /**
     * Constructs a `NonEmptySortedSet` from an `Iterable` or `None` otherwise.
     */
    def toNonEmptySortedSet: Option[NonEmptySortedSet[A]] = NonEmptySortedSet.fromIterableOption(iterable)
  }
  implicit final class NonEmptySortedSetSetOps[A](self: SortedSet[A])(implicit aOrd: SOrdering[A]) {

    /**
     * Constructs a `NonEmptySortedSet` from a `Set` or `None` otherwise.
     */
    def toNonEmptySortedSet: Option[NonEmptySortedSet[A]] = NonEmptySortedSet.fromSetOption(self)
  }
}
