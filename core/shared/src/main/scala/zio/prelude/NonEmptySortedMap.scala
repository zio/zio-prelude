package zio.prelude

/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

import zio.NonEmptyChunk

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions
import scala.math.{Ordering => SOrdering}

/**
 * A non-empty wrapper for the scala immutable map. Note - this does not attempt to implement all features of
 * map but what the author considers to be the "normal ones".
 */
case class NonEmptySortedMap[K, V] private (private val map: SortedMap[K, V])(implicit sOrdering: SOrdering[K]) {
  self =>

  private def newMap[V2](map: SortedMap[K, V2]): NonEmptySortedMap[K, V2] = new NonEmptySortedMap(map)

  /** Converts this `NonEmptySortedMap` to a `SortedMap`. */
  def toMap: SortedMap[K, V] = map

  /**
   * Returns an element of this `NonEmptySortedMap` and the remainder, which is a (possibly empty) `SortedMap`.
   */
  @inline
  def peel: ((K, V), SortedMap[K, V]) = (map.head, map.tail)

  /**
   * Returns an element of this `NonEmptySortedMap`
   * and the remainder or `None`, if the remainder is empty.
   */
  def peelNonEmpty: ((K, V), Option[NonEmptySortedMap[K, V]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(newMap(tail)))
  }

  /**
   * Creates a new `NonEmptySortedMap` with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new map that contains all elements of this map and that also
   *          contains `elem`.
   */
  def +(elem: (K, V)): NonEmptySortedMap[K, V] = newMap(map + elem)

  /**
   * Creates a new `NonEmptySortedMap` by adding all elements contained in another collection to this `NonEmptySortedMap`, omitting duplicates.
   *
   * This method takes a collection of elements and adds all elements, omitting duplicates, into `NonEmptySortedMap`.
   *
   * Example:
   *  {{{
   *    scala> val a = NonEmptySortedMap(1, 2) ++ NonEmptySortedMap(2, "a")
   *    a: zio.prelude.NonEmptySortedMap[Any] = NonEmptySortedMap(1, 2, a)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptySortedMap` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[(K, V)]): NonEmptySortedMap[K, V] = newMap(map ++ elems)

  /** Adds the `elem` to this `NonEmptySortedMap`. Alias for `+`. */
  def add(elem: (K, V)): NonEmptySortedMap[K, V] = self + elem

  /** Removes the `elem` from this `NonEmptySortedMap`. Alias for `-`. */
  def remove(elem: K): SortedMap[K, V] = map - elem

  /**
   * Returns the tail of this `NonEmptySortedMap` if it exists or `None` otherwise.
   */
  def tailNonEmpty: Option[NonEmptySortedMap[K, V]] = peelNonEmpty._2

  /**
   * Produces a new non empty map where values mapped according to function f.
   */
  def mapValues[V1](f: V => V1): NonEmptySortedMap[K, V1] = {
    val newInner = map.map(p => (p._1, f(p._2)))
    newMap(newInner)
  }

  override def hashCode: Int = map.hashCode ^ NonEmptySortedMap.NonEmptyMapSeed

  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySortedMap[_, _] => self.map == that.toMap
      case _                             => false
    }

  override def toString: String = s"NonEmpty$map"
}

object NonEmptySortedMap {

  def apply[K, V](elem: (K, V), others: Iterable[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    new NonEmptySortedMap(SortedMap(others.toList:_*) + elem)

  /**
   * Creates a `NonEmptySortedMap` with the specified elements.
   *  @tparam A      the type of the `NonEmptySortedMap`'s elements
   *  @param elem    an element of the created `NonEmptySortedMap`
   *  @param others  the remaining elements of the created `NonEmptySortedMap`
   *  @return a new `NonEmptySortedMap` with elements `elem` and `others`
   */
  def apply[K, V](elem: (K, V), others: (K, V)*)(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elem, others)

  def unapply[K, V](arg: NonEmptySortedMap[K, V]): Some[((K, V), SortedMap[K, V])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptySortedMap` from a `NonEmptyChunk`.
   */
  def fromNonEmptyChunk[K, V](elems: NonEmptyChunk[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedMap` from a `NonEmptyList`.
   */
  def fromNonEmptyList[K, V](elems: NonEmptyList[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedMap` from an element and `SortedMap`.
   */
  def fromMap[K, V](elem: (K, V), others: SortedMap[K, V])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elem, others)

  /**
   * Constructs a `NonEmptySortedMap` from a `SortedMap` or `None` otherwise.
   */
  def fromMapOption[K, V](map: SortedMap[K, V])(implicit sOrdering: SOrdering[K]): Option[NonEmptySortedMap[K, V]] =
    map.headOption.map(fromMap(_, map.tail))

  /**
   * Constructs a `NonEmptySortedMap` from an `Iterable` or `None` otherwise.
   */
  def fromIterableOption[K, V](iterable: Iterable[(K, V)])(implicit
    sOrdering: SOrdering[K]
  ): Option[NonEmptySortedMap[K, V]] =
    iterable.headOption.fold(None: Option[NonEmptySortedMap[K, V]])(h => Some(apply(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySortedMap` with the specified single value.
   */
  def single[K, V](head: (K, V))(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    NonEmptySortedMap(head)

  /**
   * Provides an implicit conversion from `NonEmptySortedMap` to the `SortedMap`
   * for interoperability with Scala's collection library.
   */
  implicit def toMap[K, V](nonEmptyMap: NonEmptySortedMap[K, V]): SortedMap[K, V] =
    nonEmptyMap.toMap

  private val NonEmptyMapSeed: Int = 1147820194

}

trait NonEmptySortedMapSyntax {
  implicit final class NonEmptySortedMapIterableOps[K, V](private val iterable: Iterable[(K, V)])(implicit
    sOrdering: SOrdering[K]
  ) {

    /**
     * Constructs a `NonEmptySortedMap` from an `Iterable` or `None` otherwise.
     */
    def toNonEmptyMap: Option[NonEmptySortedMap[K, V]] =
      NonEmptySortedMap.fromIterableOption(iterable)
  }
  implicit final class NonEmptySortedMapMapOps[K, V](self: SortedMap[K, V])(implicit sOrdering: SOrdering[K]) {

    /**
     * Constructs a `NonEmptySortedMap` from a `SortedMap` or `None` otherwise.
     */
    def toNonEmptyMap: Option[NonEmptySortedMap[K, V]] =
      NonEmptySortedMap.fromMapOption(self)
  }
}
