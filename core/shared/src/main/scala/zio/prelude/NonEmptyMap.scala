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

import scala.language.implicitConversions

/**
 * A `NonEmptyMap[K, V]` is an immutable `Map` that holds at least one
 * key-value pair. It does not implement every `Map` operation; an implicit
 * conversion to `Map` gives access to the rest, and operations that can
 * remove the last pair, such as `remove`, return a `Map`.
 *
 * {{{
 * val stock: NonEmptyMap[String, Int] = NonEmptyMap("apples" -> 3, "pears" -> 5)
 *
 * stock.mapValues(_ * 2).toMap        // Map(pears -> 10, apples -> 6)
 * stock.keySet                        // NonEmptySet(apples, pears)
 * (stock + ("plums" -> 0)).toMap.size // 3
 * }}}
 */
final class NonEmptyMap[K, V] private (private val map: Map[K, V]) { self =>

  private def newMap[V2](map: Map[K, V2]): NonEmptyMap[K, V2] = new NonEmptyMap(map)

  /** Converts this `NonEmptyMap` to a `Map`. */
  def toMap: Map[K, V] = map

  /**
   * Returns one key-value pair of this `NonEmptyMap` and the remainder, which
   * is a (possibly empty) `Map`. The pair is the first one in the iteration
   * order of the underlying `Map`.
   */
  @inline
  def peel: ((K, V), Map[K, V]) = (map.head, map.tail)

  /**
   * Returns one key-value pair of this `NonEmptyMap` and the remainder as a
   * `NonEmptyMap`, or `None` if the remainder is empty.
   */
  def peelNonEmpty: ((K, V), Option[NonEmptyMap[K, V]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(newMap(tail)))
  }

  /** Returns the keys of this `NonEmptyMap` as a `NonEmptySet`. */
  def keySet: NonEmptySet[K] = {
    val (head, tail) = peel
    NonEmptySet.fromIterable(head._1, tail.keys)
  }

  /**
   * Returns the values of this `NonEmptyMap` as a `NonEmptyChunk`, one per
   * key, so equal values appear once per key that holds them.
   */
  def values: NonEmptyChunk[V] = {
    val (head, tail) = peel
    NonEmptyChunk.fromIterable(head._2, tail.values)
  }

  /**
   * Creates a new `NonEmptyMap` with an additional key-value pair. If the key is
   *  already present, its value is replaced by the new one.
   *
   *  @param elem the key-value pair to be added
   *  @return a new map that contains all entries of this map and that also
   *          contains `elem`.
   */
  def +(elem: (K, V)): NonEmptyMap[K, V] = newMap(map + elem)

  /**
   * Creates a new `NonEmptyMap` by adding all key-value pairs contained in another collection to this `NonEmptyMap`.
   *
   * If a key is already present, the value from `elems` replaces the existing value (the later pair wins).
   *
   * Example:
   *  {{{
   *    val a = NonEmptyMap(1 -> "a", 2 -> "b") ++ List(2 -> "c", 3 -> "d")
   *    a.toMap // Map(2 -> c, 1 -> a, 3 -> d)
   *  }}}
   *
   *  @param elems     the collection containing the key-value pairs to add.
   *  @return a new `NonEmptyMap` with the given key-value pairs added, replacing values of existing keys.
   */
  def ++(elems: Iterable[(K, V)]): NonEmptyMap[K, V] = newMap(map ++ elems)

  /** Adds the `elem` to this `NonEmptyMap`. Alias for `+`. */
  def add(elem: (K, V)): NonEmptyMap[K, V] = self + elem

  /**
   * Returns the pairs of this `NonEmptyMap` without the key `elem`, as a `Map`.
   * The result is empty when `elem` is the only key.
   */
  def remove(elem: K): Map[K, V] = map - elem

  /**
   * Returns this `NonEmptyMap` without the pair that `peel` returns, or `None`
   * if that pair is the only one.
   */
  def tailNonEmpty: Option[NonEmptyMap[K, V]] = peelNonEmpty._2

  /**
   * Returns a new `NonEmptyMap` with the same keys and each value transformed
   * by `f`. The values are computed eagerly, once each.
   */
  def mapValues[V1](f: V => V1): NonEmptyMap[K, V1] = {
    val newInner = map.map(tup => tup._1 -> f(tup._2))
    newMap(newInner)
  }

  /** Returns a hash code computed from the underlying `Map`. */
  override def hashCode: Int = map.hashCode ^ NonEmptyMap.NonEmptyMapSeed

  /**
   * Returns whether `that` is a `NonEmptyMap` with the same key-value pairs.
   * A `NonEmptyMap` is never equal to a `Map`.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptyMap[_, _]       => self.map == that.toMap
      case _                             => false
    }

  /** Renders this `NonEmptyMap` as `NonEmpty` followed by the `toString` of the underlying `Map`. */
  override def toString: String = s"NonEmpty$map"
}

/**
 * Constructors and grouping operations for `NonEmptyMap`.
 */
object NonEmptyMap {

  /**
   * Creates a `NonEmptyMap` from the pair `elem` and the pairs in `others`.
   * When a key appears more than once, the pair `elem` wins over `others`,
   * and among `others` the later pair wins.
   */
  def apply[K, V](elem: (K, V), others: Iterable[(K, V)]): NonEmptyMap[K, V] =
    new NonEmptyMap(others.toMap + elem)

  /**
   * Creates a `NonEmptyMap` with the specified elements. When a key appears
   * more than once, the first pair `elem` wins over `others`:
   * `NonEmptyMap(1 -> "a", 1 -> "b")` holds `1 -> a`. Among `others` the
   * later pair wins.
   *  @tparam K      the type of the `NonEmptyMap`'s keys
   *  @tparam V      the type of the `NonEmptyMap`'s values
   *  @param elem    an element of the created `NonEmptyMap`
   *  @param others  the remaining elements of the created `NonEmptyMap`
   *  @return a new `NonEmptyMap` with elements `elem` and `others`
   */
  def apply[K, V](elem: (K, V), others: (K, V)*): NonEmptyMap[K, V] = apply(elem, others)

  /**
   * Extracts the pair and the remainder that `peel` returns, so that
   * `case NonEmptyMap(pair, rest) => ...` matches every `NonEmptyMap`.
   */
  def unapply[K, V](arg: NonEmptyMap[K, V]): Some[((K, V), Map[K, V])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptyMap` from a `NonEmptyChunk`. When a key appears
   * more than once, the head pair wins over the rest, and among the rest the
   * later pair wins.
   */
  def fromNonEmptyChunk[K, V](elems: NonEmptyChunk[(K, V)]): NonEmptyMap[K, V] = apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptyMap` from a `NonEmptyList`. When a key appears more
   * than once, the head pair wins over the rest, and among the rest the later
   * pair wins.
   */
  def fromNonEmptyList[K, V](elems: NonEmptyList[(K, V)]): NonEmptyMap[K, V] = apply(elems.head, elems.tail)

  /**
   * Constructs a `NonEmptyMap` from an element and `Map`. When `others`
   * contains the key of `elem`, the value of `elem` wins.
   */
  def fromMap[K, V](elem: (K, V), others: Map[K, V]): NonEmptyMap[K, V] = apply(elem, others)

  /**
   * Constructs a `NonEmptyMap` from a `Map`, or returns `None` when the `Map`
   * is empty.
   */
  def fromMapOption[K, V](map: Map[K, V]): Option[NonEmptyMap[K, V]] = map.headOption.map(fromMap(_, map.tail))

  /**
   * Constructs a `NonEmptyMap` from an element and `Iterable`. When a key
   * appears more than once, the pair `head` wins over `tail`, and among
   * `tail` the later pair wins.
   */
  def fromIterable[K, V](head: (K, V), tail: Iterable[(K, V)]): NonEmptyMap[K, V] =
    fromMap(head, tail.toMap)

  /**
   * Constructs a `NonEmptyMap` from an `Iterable`, or returns `None` when the
   * `Iterable` is empty. When a key appears more than once, the first pair
   * wins over the rest, and among the rest the later pair wins.
   */
  def fromIterableOption[K, V](iterable: Iterable[(K, V)]): Option[NonEmptyMap[K, V]] =
    iterable.headOption.fold(None: Option[NonEmptyMap[K, V]])(h => Some(fromIterable(h, iterable.tail)))

  /**
   * Constructs a `NonEmptyMap` with the specified single key-value pair.
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

  /**
   * Groups the elements of `from` by the key that `f` returns. Returns `None`
   * when `from` is empty, and otherwise a `NonEmptyMap` whose values are the
   * non-empty groups.
   */
  def groupByOption[A, K](from: Iterable[A])(f: A => K): Option[NonEmptyMap[K, Iterable[A]]] =
    from.headOption.map(_ => new NonEmptyMap(from.groupBy(f)))

  /**
   * Groups the elements of a `NonEmptyChunk` by the key that `f` returns.
   * Each group is a `NonEmptyChunk`.
   */
  def groupByFromNonEmptyChunk[A, K](from: NonEmptyChunk[A])(f: A => K): NonEmptyMap[K, NonEmptyChunk[A]] = {
    val gb       = from.groupBy(f)
    val asChunks = gb.map(p => (p._1 -> NonEmptyChunk.fromIterableOption(p._2).get)) // safe!
    new NonEmptyMap(asChunks)
  }

  /**
   * Groups the elements of a `NonEmptySet` by the key that `f` returns. Each
   * group is a `NonEmptySet`.
   */
  def groupByFromNonEmptySet[A, K](from: NonEmptySet[A])(f: A => K): NonEmptyMap[K, NonEmptySet[A]] = {
    val gb     = from.groupBy(f)
    val asSets = gb.map(p => (p._1 -> NonEmptySet.fromIterableOption(p._2).get)) // safe!
    new NonEmptyMap(asSets)
  }

  /**
   * Groups the elements of a `NonEmptyList` by the key that `f` returns. Each
   * group is a `NonEmptyList`.
   */
  def groupByFromNonEmptyList[A, K](from: NonEmptyList[A])(f: A => K): NonEmptyMap[K, NonEmptyList[A]] = {
    val gb      = from.groupBy(f)
    val asLists = gb.map(p => (p._1 -> NonEmptyList.fromIterableOption(p._2).get)) // safe!
    new NonEmptyMap(asLists)
  }

}

/**
 * Extension methods that convert Scala collections of pairs to `NonEmptyMap`.
 * `import zio.prelude._` does not include them; mix this trait into an object
 * and import its members to use them.
 */
trait NonEmptyMapSyntax {

  /** Adds `toNonEmptyMap` to every `Iterable` of key-value pairs. */
  implicit final class NonEmptyMapIterableOps[K, V](private val iterable: Iterable[(K, V)]) {

    /**
     * Constructs a `NonEmptyMap` from an `Iterable`, or returns `None` when
     * the `Iterable` is empty.
     */
    def toNonEmptyMap: Option[NonEmptyMap[K, V]] = NonEmptyMap.fromIterableOption(iterable)
  }

  /** Adds `toNonEmptyMap` to every `Map`. */
  implicit final class NonEmptyMapMapOps[K, V](self: Map[K, V]) {

    /**
     * Constructs a `NonEmptyMap` from a `Map`, or returns `None` when the
     * `Map` is empty.
     */
    def toNonEmptyMap: Option[NonEmptyMap[K, V]] = NonEmptyMap.fromMapOption(self)
  }
}
