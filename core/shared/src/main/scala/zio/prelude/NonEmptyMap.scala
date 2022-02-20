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
import scala.language.implicitConversions

/**
 * A non-empty wrapper for the scala immutable map. Note - this does not attempt to implement all features of
 * map but what the author considers to be the "normal ones".
 */
case class NonEmptyMap[K, V] private(private val map: Map[K, V]) { self =>

  private def newMap[V2](map: Map[K, V2]): NonEmptyMap[K,V2] = new NonEmptyMap(map)

  /** Converts this `NonEmptyMap` to a `Map`. */
  def toMap: Map[K, V] = map

  /**
   * Returns an element of this `NonEmptyMap` and the remainder, which is a (possibly empty) `Map`.
   */
  @inline
  def peel: ((K, V), Map[K, V]) = (map.head, map.tail)

  /**
   * Returns an element of this `NonEmptyMap`
   * and the remainder or `None`, if the remainder is empty.
   */
  def peelNonEmpty: ((K, V), Option[NonEmptyMap[K, V]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(newMap(tail)))
  }


  /**
   * Creates a new `NonEmptyMap` with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new map that contains all elements of this map and that also
   *          contains `elem`.
   */
  def +(elem: (K,V)): NonEmptyMap[K, V] = newMap(map + elem)

  /**
   * Creates a new `NonEmptyMap` by adding all elements contained in another collection to this `NonEmptyMap`, omitting duplicates.
   *
   * This method takes a collection of elements and adds all elements, omitting duplicates, into `NonEmptyMap`.
   *
   * Example:
   *  {{{
   *    scala> val a = NonEmptyMap(1, 2) ++ NonEmptyMap(2, "a")
   *    a: zio.prelude.NonEmptyMap[Any] = NonEmptyMap(1, 2, a)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptyMap` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[(K, V)]): NonEmptyMap[K, V] = newMap(map ++ elems)

  /** Adds the `elem` to this `NonEmptyMap`. Alias for `+`. */
  def add(elem: (K, V)): NonEmptyMap[K, V] = self + elem

  /** Removes the `elem` from this `NonEmptyMap`. Alias for `-`. */
  def remove(elem: K): Map[K, V] = map - elem

  /**
   * Returns the tail of this `NonEmptyMap` if it exists or `None` otherwise.
   */
  def tailNonEmpty: Option[NonEmptyMap[K, V]] = peelNonEmpty._2

  /**
   * Produces a new non empty map where values mapped according to function f. Does not use map.view
   * NB. Scala collections 2.13 deprecated this on Map but promises to return it in "a future version"
   */
  def mapValues[V1](f: V => V1): NonEmptyMap[K, V1] = {
    val newInner = map.view.mapValues(f).toMap // see
    newMap(newInner)
  }

  override def hashCode: Int = map.hashCode ^ NonEmptyMap.NonEmptyMapSeed

  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptyMap[_, _]          => self.map == that.toMap
      case _                             => false
    }

  override def toString: String = s"NonEmpty$map"
}

object NonEmptyMap {

  def apply[K, V](elem: (K, V), others: Iterable[(K, V)]): NonEmptyMap[K, V] =
    new NonEmptyMap(others.toMap + elem)

  /**
   * Creates a `NonEmptyMap` with the specified elements.
   *  @tparam A      the type of the `NonEmptyMap`'s elements
   *  @param elem    an element of the created `NonEmptyMap`
   *  @param others  the remaining elements of the created `NonEmptyMap`
   *  @return a new `NonEmptyMap` with elements `elem` and `others`
   */
  def apply[K, V](elem: (K, V), others: (K, V)*): NonEmptyMap[K, V] = apply(elem, others)

  def unapply[K, V](arg: NonEmptyMap[K, V]): Some[((K, V), Map[K, V])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptyMap` from a `NonEmptyChunk`.
   */
  def fromNonEmptyChunk[K, V](elems: NonEmptyChunk[(K, V)]): NonEmptyMap[K, V] = apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptyMap` from a `NonEmptyList`.
   */
  def fromNonEmptyList[K, V](elems: NonEmptyList[(K, V)]): NonEmptyMap[K, V] = apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptyMap` from an element and `Map`.
   */
  def fromMap[K, V](elem: (K, V), others: Map[K, V]): NonEmptyMap[K, V] = apply(elem, others)

  /**
   * Constructs a `NonEmptyMap` from a `Map` or `None` otherwise.
   */
  def fromMapOption[K, V](map: Map[K, V]): Option[NonEmptyMap[K, V]] = map.headOption.map(fromMap(_, map.tail))

  /**
   * Constructs a `NonEmptyMap` from an element and `Iterable`.
   */
  def fromIterable[K, V](head: (K, V), tail: Iterable[(K, V)]): NonEmptyMap[K, V] =
    fromMap(head, tail.toMap)

  /**
   * Constructs a `NonEmptyMap` from an `Iterable` or `None` otherwise.
   */
  def fromIterableOption[K, V](iterable: Iterable[(K, V)]): Option[NonEmptyMap[K, V]] =
    iterable.headOption.fold(None: Option[NonEmptyMap[K, V]])(h => Some(fromIterable(h, iterable.tail)))

  /**
   * Constructs a `NonEmptyMap` with the specified single value.
   */
  def single[K, V](head: (K, V)): NonEmptyMap[K, V] =
    NonEmptyMap(head)

  /**
   * Provides an implicit conversion from `NonEmptyMap` to the `Map`
   * for interoperability with Scala's collection library.
   */
  implicit def toMap[K, V](nonEmptyMap: NonEmptyMap[K, V]): Map[K, V] =
    nonEmptyMap.toMap

  private val NonEmptyMapSeed: Int = 1247820194

}

trait NonEmptyMapSyntax {
  implicit final class NonEmptyMapIterableOps[K, V](private val iterable: Iterable[(K, V)]) {

    /**
     * Constructs a `NonEmptyMap` from an `Iterable` or `None` otherwise.
     */
    def toNonEmptyMap: Option[NonEmptyMap[K, V]] = NonEmptyMap.fromIterableOption(iterable)
  }
  implicit final class NonEmptyMapMapOps[K, V](self: Map[K, V]) {

    /**
     * Constructs a `NonEmptyMap` from a `Map` or `None` otherwise.
     */
    def toNonEmptyMap: Option[NonEmptyMap[K, V]] = NonEmptyMap.fromMapOption(self)
  }
}

