package zio.prelude

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

import zio.NonEmptyChunk

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions
import scala.math.{Ordering => SOrdering}

/**
 * A `NonEmptySortedMap[K, V]` is an immutable `SortedMap` that holds at least
 * one key-value pair, with keys kept in the order of a
 * `scala.math.Ordering[K]`. It does not implement every `SortedMap`
 * operation; an implicit conversion to `SortedMap` gives access to the rest,
 * and operations that can remove the last pair, such as `remove`, return a
 * `SortedMap`.
 *
 * {{{
 * val rooms: NonEmptySortedMap[Int, String] = NonEmptySortedMap(204 -> "Ada", 101 -> "Grace")
 *
 * rooms.peel._1                 // (101,Grace)
 * rooms.keySet.toSet            // TreeSet(101, 204)
 * rooms.remove(101).keys.toList // List(204)
 * }}}
 */
final class NonEmptySortedMap[K, V] private (private val map: SortedMap[K, V])(implicit sOrdering: SOrdering[K]) {
  self =>

  private def newMap[V2](map: SortedMap[K, V2]): NonEmptySortedMap[K, V2] = new NonEmptySortedMap(map)

  /** Converts this `NonEmptySortedMap` to a `SortedMap`. */
  def toMap: SortedMap[K, V] = map

  /**
   * Returns the pair with the smallest key in this `NonEmptySortedMap` and the remainder, which is a (possibly empty) `SortedMap`.
   */
  @inline
  def peel: ((K, V), SortedMap[K, V]) = (map.head, map.tail)

  /**
   * Returns the pair with the smallest key in this `NonEmptySortedMap` and
   * the remainder as a `NonEmptySortedMap`, or `None` if the remainder is
   * empty.
   */
  def peelNonEmpty: ((K, V), Option[NonEmptySortedMap[K, V]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(newMap(tail)))
  }

  /** Returns the keys of this `NonEmptySortedMap` as a `NonEmptySortedSet`. */
  def keySet: NonEmptySortedSet[K] = {
    val (head, tail) = peel
    NonEmptySortedSet(head._1, tail.keySet)
  }

  /**
   * Returns the values of this `NonEmptySortedMap` as a `NonEmptyChunk`, in
   * the order of their keys.
   */
  def values: NonEmptyChunk[V] = {
    val (head, tail) = peel
    NonEmptyChunk.fromIterable(head._2, tail.values)
  }

  /**
   * Creates a new `NonEmptySortedMap` with an additional key-value pair. If the key is
   *  already present, its value is replaced by the new one.
   *
   *  @param elem the key-value pair to be added
   *  @return a new map that contains all entries of this map and that also
   *          contains `elem`.
   */
  def +(elem: (K, V)): NonEmptySortedMap[K, V] = newMap(map + elem)

  /**
   * Creates a new `NonEmptySortedMap` by adding all key-value pairs contained in another collection to this `NonEmptySortedMap`.
   *
   * If a key is already present, the value from `elems` replaces the existing value (the later pair wins).
   *
   * Example:
   *  {{{
   *    val a = NonEmptySortedMap(1 -> "a", 2 -> "b") ++ List(2 -> "c", 3 -> "d")
   *    a.toMap.toList // List((1,a), (2,c), (3,d))
   *  }}}
   *
   *  @param elems     the collection containing the key-value pairs to add.
   *  @return a new `NonEmptySortedMap` with the given key-value pairs added, replacing values of existing keys.
   */
  def ++(elems: Iterable[(K, V)]): NonEmptySortedMap[K, V] = newMap(map ++ elems)

  /** Adds the `elem` to this `NonEmptySortedMap`. Alias for `+`. */
  def add(elem: (K, V)): NonEmptySortedMap[K, V] = self + elem

  /**
   * Returns the pairs of this `NonEmptySortedMap` without the key `elem`, as
   * a `SortedMap`. The result is empty when `elem` is the only key.
   */
  def remove(elem: K): SortedMap[K, V] = map - elem

  /**
   * Returns this `NonEmptySortedMap` without the pair with the smallest key,
   * or `None` if that pair is the only one.
   */
  def tailNonEmpty: Option[NonEmptySortedMap[K, V]] = peelNonEmpty._2

  /**
   * Returns a new `NonEmptySortedMap` with the same keys and each value
   * transformed by `f`.
   */
  def mapValues[V1](f: V => V1): NonEmptySortedMap[K, V1] = {
    val newInner = map.map(p => (p._1, f(p._2)))
    newMap(newInner)
  }

  /** Returns a hash code computed from the underlying `SortedMap`. */
  override def hashCode: Int = map.hashCode ^ NonEmptySortedMap.NonEmptyMapSeed

  /**
   * Returns whether `that` is a `NonEmptySortedMap` with the same key-value
   * pairs. A `NonEmptySortedMap` is never equal to a `SortedMap`.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySortedMap[_, _] => self.map == that.toMap
      case _                             => false
    }

  /**
   * Renders this `NonEmptySortedMap` as `NonEmpty` followed by the `toString`
   * of the underlying `SortedMap`, for example `NonEmptyTreeMap(1 -> a)`.
   */
  override def toString: String = s"NonEmpty$map"
}

/**
 * Constructors for `NonEmptySortedMap`. Every constructor sorts the keys with
 * the implicit `scala.math.Ordering` it is given.
 */
object NonEmptySortedMap {

  /**
   * Creates a `NonEmptySortedMap` from the pair `elem` and the pairs in
   * `others`. When a key appears more than once, the pair `elem` wins over
   * `others`, and among `others` the later pair wins.
   */
  def apply[K, V](elem: (K, V), others: Iterable[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    new NonEmptySortedMap(SortedMap(others.toList: _*) + elem)

  /**
   * Creates a `NonEmptySortedMap` with the specified elements. When a key
   * appears more than once, the first pair `elem` wins over `others`:
   * `NonEmptySortedMap(1 -> "a", 1 -> "b")` holds `1 -> a`. Among `others`
   * the later pair wins.
   *  @tparam K      the type of the `NonEmptySortedMap`'s keys
   *  @tparam V      the type of the `NonEmptySortedMap`'s values
   *  @param elem    an element of the created `NonEmptySortedMap`
   *  @param others  the remaining elements of the created `NonEmptySortedMap`
   *  @return a new `NonEmptySortedMap` with elements `elem` and `others`
   */
  def apply[K, V](elem: (K, V), others: (K, V)*)(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elem, others)

  /**
   * Extracts the pair with the smallest key and the remainder, as `peel`
   * does, so that `case NonEmptySortedMap(first, rest) => ...` matches every
   * `NonEmptySortedMap`.
   */
  def unapply[K, V](arg: NonEmptySortedMap[K, V]): Some[((K, V), SortedMap[K, V])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptySortedMap` from a `NonEmptyChunk`. When a key
   * appears more than once, the head pair wins over the rest, and among the
   * rest the later pair wins.
   */
  def fromNonEmptyChunk[K, V](elems: NonEmptyChunk[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedMap` from a `NonEmptyList`. When a key
   * appears more than once, the head pair wins over the rest, and among the
   * rest the later pair wins.
   */
  def fromNonEmptyList[K, V](elems: NonEmptyList[(K, V)])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptySortedMap` from an element and `SortedMap`. When
   * `others` contains the key of `elem`, the value of `elem` wins. The result
   * is sorted by the implicit `sOrdering`, not by the ordering of `others`.
   */
  def fromMap[K, V](elem: (K, V), others: SortedMap[K, V])(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    apply(elem, others)

  /**
   * Constructs a `NonEmptySortedMap` from a `SortedMap`, or returns `None`
   * when the `SortedMap` is empty. The result is sorted by the implicit
   * `sOrdering`, not by the ordering of `map`.
   */
  def fromMapOption[K, V](map: SortedMap[K, V])(implicit sOrdering: SOrdering[K]): Option[NonEmptySortedMap[K, V]] =
    map.headOption.map(fromMap(_, map.tail))

  /**
   * Constructs a `NonEmptySortedMap` from an `Iterable`, or returns `None`
   * when the `Iterable` is empty. When a key appears more than once, the first
   * pair wins over the rest, and among the rest the later pair wins.
   */
  def fromIterableOption[K, V](iterable: Iterable[(K, V)])(implicit
    sOrdering: SOrdering[K]
  ): Option[NonEmptySortedMap[K, V]] =
    iterable.headOption.fold(None: Option[NonEmptySortedMap[K, V]])(h => Some(apply(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySortedMap` with the specified single key-value pair.
   */
  def single[K, V](head: (K, V))(implicit sOrdering: SOrdering[K]): NonEmptySortedMap[K, V] =
    NonEmptySortedMap(head)

  /**
   * Provides an implicit conversion from `NonEmptySortedMap` to `SortedMap`
   * for interoperability with Scala's collection library.
   */
  implicit def toMap[K, V](nonEmptyMap: NonEmptySortedMap[K, V]): SortedMap[K, V] =
    nonEmptyMap.toMap

  private val NonEmptyMapSeed: Int = 1147820194

}

/**
 * Extension methods that convert Scala collections of pairs to
 * `NonEmptySortedMap`. `import zio.prelude._` does not include them; mix this
 * trait into an object and import its members to use them.
 */
trait NonEmptySortedMapSyntax {

  /**
   * Adds `toNonEmptyMap`, which builds a `NonEmptySortedMap`, to every
   * `Iterable` of pairs whose key type has a `scala.math.Ordering`.
   */
  implicit final class NonEmptySortedMapIterableOps[K, V](private val iterable: Iterable[(K, V)])(implicit
    sOrdering: SOrdering[K]
  ) {

    /**
     * Constructs a `NonEmptySortedMap` from an `Iterable`, or returns `None`
     * when the `Iterable` is empty.
     */
    def toNonEmptyMap: Option[NonEmptySortedMap[K, V]] =
      NonEmptySortedMap.fromIterableOption(iterable)
  }

  /**
   * Adds `toNonEmptyMap`, which builds a `NonEmptySortedMap`, to every
   * `SortedMap` whose key type has a `scala.math.Ordering`.
   */
  implicit final class NonEmptySortedMapMapOps[K, V](self: SortedMap[K, V])(implicit sOrdering: SOrdering[K]) {

    /**
     * Constructs a `NonEmptySortedMap` from a `SortedMap`, or returns `None`
     * when the `SortedMap` is empty.
     */
    def toNonEmptyMap: Option[NonEmptySortedMap[K, V]] =
      NonEmptySortedMap.fromMapOption(self)
  }
}
