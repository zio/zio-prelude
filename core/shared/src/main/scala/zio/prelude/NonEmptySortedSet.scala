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

/**
 * A `NonEmptySortedSet[A]` is an immutable `SortedSet` that holds at least
 * one element, kept in the order of a `scala.math.Ordering[A]`. Operations
 * that can remove every element, such as `remove`, return a `SortedSet`; an
 * implicit conversion to `SortedSet` gives access to the other `SortedSet`
 * operations.
 *
 * {{{
 * val levels: NonEmptySortedSet[Int] = NonEmptySortedSet(30, 10, 20)
 *
 * levels.peel           // (10,TreeSet(20, 30))
 * (levels + 5).toSet    // TreeSet(5, 10, 20, 30)
 * levels.map(_ / 10).toSet // TreeSet(1, 2, 3)
 * }}}
 */
final class NonEmptySortedSet[A] private (private val set: SortedSet[A]) { self =>

  /** Converts this `NonEmptySortedSet` to a `SortedSet`. */
  def toSet: SortedSet[A] = set

  /**
   * Returns the smallest element of this `NonEmptySortedSet` and the remainder, which is a (possibly empty)
   * `SortedSet`.
   */
  @inline
  def peel: (A, SortedSet[A]) = (set.head, set.tail)

  /**
   * Returns the smallest element of this `NonEmptySortedSet` and the
   * remainder as a `NonEmptySortedSet`, or `None` if the remainder is empty.
   */
  def peelNonEmpty: (A, Option[NonEmptySortedSet[A]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(new NonEmptySortedSet(tail)))
  }

  /**
   * Converts this `NonEmptySortedSet` to a `NonEmptyChunk`, in sorted order.
   */
  def toNonEmptyChunk: NonEmptyChunk[A] = peel match { case (head, tail) => NonEmptyChunk.fromIterable(head, tail) }

  /**
   * Converts this `NonEmptySortedSet` to a `NonEmptyList`, in sorted order.
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
   * Computes the union of this `NonEmptySortedSet` and another set. The
   * result keeps the ordering of this set.
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
   *    val a = NonEmptySortedSet(1, 2) ++ List(2, 0)
   *    a.toSet // TreeSet(0, 1, 2)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptySortedSet` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[A]): NonEmptySortedSet[A] = new NonEmptySortedSet(set ++ elems)

  /** Adds the `elem` to this `NonEmptySortedSet`. Alias for `+`. */
  def add(elem: A): NonEmptySortedSet[A] = self + elem

  /**
   * Returns the elements of this `NonEmptySortedSet` other than `elem`, as a
   * `SortedSet`. The result is empty when `elem` is the only element.
   */
  def remove(elem: A): SortedSet[A] = set - elem

  /**
   * Removes `elem` from this `NonEmptySortedSet`, returning the rest as a
   * `NonEmptySortedSet`, or `None` if nothing is left.
   */
  def removeNonEmpty(elem: A): Option[NonEmptySortedSet[A]] = {
    val newSet = set - elem
    if (newSet.nonEmpty) Some(new NonEmptySortedSet(newSet)) else None
  }

  /**
   * Returns the elements except the smallest one, as a `NonEmptySortedSet`,
   * or `None` if there is only one element.
   */
  def tailNonEmpty: Option[NonEmptySortedSet[A]] = peelNonEmpty._2

  /**
   * Returns a `NonEmptySortedSet` of the results of applying `f` to each
   * element, sorted by the given `scala.math.Ordering[B]`. Results that are
   * equal are kept once.
   */
  def map[B](f: A => B)(implicit sOrdering: SOrdering[B]): NonEmptySortedSet[B] = new NonEmptySortedSet(set.map(f))

  /** Returns a hash code computed from the underlying `SortedSet`. */
  override def hashCode: Int = set.hashCode ^ NonEmptySortedSet.NonEmptySortedSetSeed

  /**
   * Returns whether `that` is a `NonEmptySortedSet` with the same elements.
   * A `NonEmptySortedSet` is never equal to a `SortedSet` or a `NonEmptySet`.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySortedSet[_]    => self.set == that.toSet
      case _                             => false
    }

  /**
   * Renders this `NonEmptySortedSet` as `NonEmpty` followed by the `toString`
   * of the underlying `SortedSet`, for example `NonEmptyTreeSet(1, 2, 3)`.
   */
  override def toString: String = s"NonEmpty$set"
}

/**
 * Constructors and type class instances for `NonEmptySortedSet`. Every
 * constructor sorts the elements with the implicit `scala.math.Ordering`
 * it is given.
 */
object NonEmptySortedSet {

  /** Creates a `NonEmptySortedSet` from `elem` and the elements of `others`. */
  def apply[A](elem: A, others: Iterable[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] = {
    val treeSet = new TreeSet() + elem ++ others
    new NonEmptySortedSet(treeSet)
  }

  /** Creates a `NonEmptySortedSet` with the specified elements. */
  def apply[A](elem: A, others: A*)(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elem, others)

  /**
   * Extracts the smallest element and the remainder, as `peel` does, so that
   * `case NonEmptySortedSet(min, rest) => ...` matches every
   * `NonEmptySortedSet`.
   */
  def unapply[A](arg: NonEmptySortedSet[A]): Some[(A, SortedSet[A])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptySortedSet` from a `NonEmptyChunk`.
   */
  def fromNonEmptyChunk[A](elems: NonEmptyChunk[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedSet` from a `NonEmptyList`.
   */
  def fromNonEmptyList[A](elems: NonEmptyList[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedSet` from an element and `SortedSet`. The
   * result is sorted by the implicit `ordering`, not by the ordering of
   * `others`.
   */
  def fromSet[A](elem: A, others: SortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(elem, others)

  /**
   * Constructs a `NonEmptySortedSet` from a `SortedSet`, or returns `None`
   * when the `SortedSet` is empty. The result is sorted by the implicit
   * `ordering`, not by the ordering of `set`.
   */
  def fromSetOption[A](set: SortedSet[A])(implicit ordering: SOrdering[A]): Option[NonEmptySortedSet[A]] =
    set.headOption.map(fromSet(_, set.tail))

  /**
   * Constructs a `NonEmptySortedSet` from an `Iterable`, or returns `None`
   * when the `Iterable` is empty.
   */
  def fromIterableOption[A](iterable: Iterable[A])(implicit ordering: SOrdering[A]): Option[NonEmptySortedSet[A]] =
    iterable.headOption.fold(None: Option[NonEmptySortedSet[A]])(h => Some(apply(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySortedSet` with the specified single value.
   */
  def single[A](head: A)(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    apply(head)

  /**
   * Creates a `NonEmptySortedSet` containing elements from `l` and `r`,
   * sorted by the implicit `ordering`.
   */
  def union[A](l: NonEmptySortedSet[A], r: SortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] = {
    val (head, tail) = l.peel
    NonEmptySortedSet.fromSet(head, tail.union(r))
  }

  /**
   * Creates a `NonEmptySortedSet` containing elements from `l` and `r`,
   * sorted by the implicit `ordering`.
   */
  def union[A](l: SortedSet[A], r: NonEmptySortedSet[A])(implicit ordering: SOrdering[A]): NonEmptySortedSet[A] =
    union(r, l)

  /**
   * The `Commutative` and `Idempotent` (and thus `Associative`) instance for `NonEmptySortedSet`,
   * which combines two sets by union.
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
   * The `DeriveEqual` instance for `NonEmptySortedSet`. The derived `Equal`
   * does not use the `Equal[A]` it is given: it compares the sets with `==`.
   */
  implicit val NonEmptySortedSetDeriveEqual: DeriveEqual[NonEmptySortedSet] =
    new DeriveEqual[NonEmptySortedSet] {
      def derive[A: Equal]: Equal[NonEmptySortedSet[A]] =
        NonEmptySortedSetHashPartialOrd
    }

  /**
   * Derives a `Hash[NonEmptySortedSet[A]]` and `PartialOrd[NonEmptySortedSet[A]]` (and thus
   * `Equal[NonEmptySortedSet[A]]`) instance. It needs no instance for `A`: membership is checked with each set's
   * `scala.math.Ordering`, and elements are hashed with `hashCode`. The partial order is set inclusion.
   */
  implicit def NonEmptySortedSetHashPartialOrd[A]: Hash[NonEmptySortedSet[A]] with PartialOrd[NonEmptySortedSet[A]] =
    HashPartialOrd.derive[Set[A]].contramap(_.toSet)

  /**
   * Provides an implicit conversion from `NonEmptySortedSet` to `SortedSet`
   * for interoperability with Scala's collection library.
   */
  implicit def toSet[A](nonEmptySet: NonEmptySortedSet[A]): SortedSet[A] =
    nonEmptySet.toSet

  private val NonEmptySortedSetSeed: Int = 1247120194

}

/**
 * Extension methods that convert Scala collections to `NonEmptySortedSet`.
 * `import zio.prelude._` does not include them; mix this trait into an object
 * and import its members to use them.
 */
trait NonEmptySortedSetSyntax {

  /**
   * Adds `toNonEmptySortedSet` to every `Iterable` whose element type has a
   * `scala.math.Ordering`.
   */
  implicit final class NonEmptySortedSetIterableOps[A](private val iterable: Iterable[A])(implicit aOrd: SOrdering[A]) {

    /**
     * Constructs a `NonEmptySortedSet` from an `Iterable`, or returns `None`
     * when the `Iterable` is empty.
     */
    def toNonEmptySortedSet: Option[NonEmptySortedSet[A]] = NonEmptySortedSet.fromIterableOption(iterable)
  }

  /**
   * Adds `toNonEmptySortedSet` to every `SortedSet` whose element type has a
   * `scala.math.Ordering`.
   */
  implicit final class NonEmptySortedSetSetOps[A](self: SortedSet[A])(implicit aOrd: SOrdering[A]) {

    /**
     * Constructs a `NonEmptySortedSet` from a `SortedSet`, or returns `None`
     * when the `SortedSet` is empty.
     */
    def toNonEmptySortedSet: Option[NonEmptySortedSet[A]] = NonEmptySortedSet.fromSetOption(self)
  }
}
