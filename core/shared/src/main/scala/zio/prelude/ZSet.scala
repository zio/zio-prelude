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

import zio.prelude.newtypes._

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.HashMap

/**
 * A `ZSet[A, B]` is a set of `A` values where `B` represents some notion of
 * "how many" `A` values are included in the set. This can be the number of
 * times each element appears in the set if `B` is a natural number, the
 * probability associated with an element in the set if `B` is a rational
 * number, or even whether an element appears at all if `B` is a boolean.
 *
 * A `MultiSet[A]` is a `ZSet[A, Natural]`. The set operations combine the
 * measures with the `Sum`, `Prod`, `Min` and `Max` instances for `B`:
 *
 * {{{
 * val words  = ZSet("apple", "apple", "pear")
 * val basket = ZSet("apple", "plum")
 * val both   = words <> basket
 *
 * words("apple")
 * // 2
 *
 * both("apple")
 * // 3
 *
 * (words & basket).toSet
 * // Set(apple)
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/functional-data-types/zset/ ZSet]]
 */
final class ZSet[+A, +B] private (private val map: HashMap[A @uncheckedVariance, B]) { self =>

  /**
   * A symbolic alias for `intersect`.
   */
  def &[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Min[B1]]): ZSet[A1, B1] =
    self intersect that

  /**
   * A symbolic alias for `diff`.
   */
  def &~[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Inverse[Sum[B1]]): ZSet[A1, B1] =
    self diff that

  /**
   * A symbolic alias for `zip`.
   */
  def <*>[B1 >: B, C](that: ZSet[C, B1])(implicit ev: Commutative[Prod[B1]]): ZSet[(A, C), B1] =
    self zip that

  /**
   * A symbolic alias for `combine`.
   */
  def <>[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZSet[A1, B1] =
    self combine that

  /**
   * A symbolic alias for `union`.
   */
  def |[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev1: Commutative[Max[B1]]): ZSet[A1, B1] =
    self union that

  /**
   * Returns the number of times the specified element appears in the set, or
   * the identity of `Sum[B1]` (zero for numbers) if it does not appear.
   */
  def apply[A1 >: A, B1 >: B](a: A1)(implicit ev: Identity[Sum[B1]]): B1 =
    map.asInstanceOf[Map[A1, B1]].getOrElse(a, ev.identity)

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the sum of the number of times it
   * appears in this set and the specified set.
   */
  def combine[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZSet[A1, B1] =
    new ZSet(that.map.foldLeft(self.map.asInstanceOf[HashMap[A1, B1]]) { case (map, (a, b1)) =>
      map.get(a) match {
        case Some(b) => map + (a -> ev.combine(Sum(b), Sum(b1)))
        case None    => map + (a -> b1)
      }
    })

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the difference between the number
   * of times it appears in this set and the specified set. An element that
   * appears only in the specified set gets the inverse of its count, so with
   * `Int` counts it is negative.
   *
   * {{{
   * val diff = ZSet("a", "a", "b") &~ ZSet("a", "c")
   *
   * diff("a")
   * // 1
   *
   * diff("c")
   * // -1
   * }}}
   */
  def diff[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Inverse[Sum[B1]]): ZSet[A1, B1] =
    new ZSet(that.map.foldLeft(self.map.asInstanceOf[HashMap[A1, B1]]) { case (map, (a, b1)) =>
      map.get(a) match {
        case Some(b) => map + (a -> ev.inverse(Sum(b), Sum(b1)))
        case None    => map + (a -> ev.inverse(ev.identity, Sum(b1)))
      }
    })

  /**
   * Returns whether this set is equal to the specified set, meaning that the
   * same elements appear in both sets the same number of times. An element
   * stored with a zero count is not equal to an absent element here; the
   * `Equal` instance (`===`) ignores entries whose count is zero.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: ZSet[_, _]              => self.map == that.map
      case _                             => false
    }

  /**
   * Creates a new set for each element in this set and combines the resulting
   * sets together. The number of times each element appears will be the sum
   * of the products of the number of times it appeared in the original set
   * and the number of times it appears in each new set.
   */
  def flatMap[B1 >: B, C](
    f: A => ZSet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZSet[C, B1] =
    map.foldLeft[ZSet[C, B1]](ZSet.empty) { case (set, (a, b)) =>
      set <> f(a).transform(b1 => ev2.combine(Prod(b), Prod(b1)))
    }

  /**
   * Applies the effectual function `f` once for each occurrence of each
   * element of this `MultiSet`, and collects the results into a `MultiSet`
   * that counts how many times each result was produced.
   */
  def forEach[G[+_]: IdentityBoth: Covariant, C](f: A => G[C])(implicit ev: B <:< Natural): G[MultiSet[C]] = {

    @tailrec
    def loop(g: G[HashMap[C, Natural]], a: A, n: Int): G[HashMap[C, Natural]] =
      if (n <= 0) g
      else {
        val updated = g.zipWith(f(a)) { (map, c) =>
          map.get(c).fold(map.updated(c, Natural.one))(n => map.updated(c, Natural.successor(n)))
        }
        loop(updated, a, n - 1)
      }

    map
      .foldLeft[G[HashMap[C, Natural]]](HashMap.empty.succeed) { case (g, (a, b)) => loop(g, a, ev(b)) }
      .map(new ZSet(_))
  }

  /**
   * Returns the hash code of this set.
   */
  override def hashCode: Int =
    map.hashCode

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the minimum of the number of times
   * it appears in this set and the specified set.
   */
  def intersect[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Min[B1]]): ZSet[A1, B1] =
    new ZSet(self.map.foldLeft[HashMap[A1, B1]](HashMap.empty) { case (map, (a, b)) =>
      that.map.get(a) match {
        case Some(b1) => map + (a -> ev.combine(Min(b), Min(b1)))
        case None     => map
      }
    })

  /**
   * Transforms the elements in the set using the specified function. If this
   * results in mapping two or more elements to the same values, the number of
   * times the new value appears in the set will be the sum of the number of
   * times each of the old values appeared in the set.
   */
  def map[B1 >: B, C](f: A => C)(implicit ev: Commutative[Sum[B1]]): ZSet[C, B1] =
    new ZSet(map.foldLeft[HashMap[C, B1]](HashMap.empty) { case (map, (a, b1)) =>
      val c = f(a)
      map.get(c) match {
        case Some(b) => map + (c -> ev.combine(Sum(b), Sum(b1)))
        case None    => map + (c -> b1)
      }
    })

  /**
   * Transforms the representation of how many times each element appears in
   * the set with the specified function.
   */
  def transform[C](f: B => C): ZSet[A, C] =
    new ZSet(map.map { case (a, b) => (a, f(b)) })

  /**
   * Converts this set to a `Map` from elements to how many times they appear
   * in the set, including any entries with a zero count.
   */
  def toMap[A1 >: A]: Map[A1, B] =
    map.asInstanceOf[Map[A1, B]]

  /**
   * Converts this set to a `ZNonEmptySet`, or returns `None` if the set has no
   * entries. An entry with a zero count still counts as an entry.
   */
  def toNonEmptyZSet: Option[ZNonEmptySet[A, B]] = ZNonEmptySet.fromMapOption(map)

  /**
   * Converts this set to a `Set`, discarding information about how many times
   * an element appears in the set beyond whether it appears at all.
   */
  def toSet[A1 >: A, B1 >: B](implicit ev1: Equal[B1], ev2: Identity[Sum[B1]]): Set[A1] =
    map.foldLeft(Set.empty[A1]) { case (set, (a, b)) =>
      if (ev1.notEqual(b, ev2.identity)) set + a else set
    }

  /**
   * Returns a meaningful string representation of this set.
   */
  override def toString: String =
    map.mkString("ZSet(", ", ", ")")

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the maximum of the number of times
   * it appears in this set and the specified set.
   */
  def union[A1 >: A, B1 >: B](
    that: ZSet[A1, B1]
  )(implicit ev: Commutative[Max[B1]]): ZSet[A1, B1] =
    new ZSet(self.map.foldLeft(that.map) { case (map, (a, b)) =>
      map.get(a) match {
        case Some(b1) => map + (a -> ev.combine(Max(b), Max(b1)))
        case None     => map + (a -> b)
      }
    })

  /**
   * Combines this set with the specified set to produce their cartesian
   * product. The count of each pair is the product of the counts of its
   * elements.
   */
  def zip[B1 >: B, C](
    that: ZSet[C, B1]
  )(implicit ev: Commutative[Prod[B1]]): ZSet[(A, C), B1] =
    new ZSet(self.map.flatMap { case (a, b) =>
      that.map.map { case (c, b1) => ((a, c), ev.combine(Prod(b), Prod(b1))) }
    })

  /**
   * Combines this set with the specified set to produce their cartesian
   * product, combining pair of elements using the specified function `f`.
   *
   * {{{
   * val die  = ZSet(1, 2, 3, 4, 5, 6)
   * val sums = die.zipWith(die)(_ + _)
   *
   * sums(7)
   * // 6
   * }}}
   */
  def zipWith[B1 >: B, C, D](
    that: ZSet[C, B1]
  )(f: (A, C) => D)(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZSet[D, B1] =
    self.flatMap(a => that.map(c => f(a, c)))
}

/**
 * Constructors for [[ZSet]] values and its type class instances. It is also
 * available under the name `MultiSet`.
 */
object ZSet extends LowPriorityZSetImplicits {

  /**
   * Constructs a set with the specified elements, counting how many times
   * each one occurs.
   */
  def apply[A](as: A*): ZSet[A, Int] =
    fromIterable(as)

  /**
   * The empty set.
   */
  val empty: ZSet[Nothing, Nothing] =
    new ZSet(HashMap.empty)

  /**
   * Constructs a set from the specified `Iterable`. The measure of how many
   * times a value occurs in the set will be an integer representing how many
   * times the value occurred in the specified `Iterable`.
   */
  def fromIterable[A](iterable: Iterable[A]): MultiSet[A] =
    new ZSet(iterable.foldLeft(HashMap.empty[A, Natural]) { (map, a) =>
      map + (a -> map.get(a).fold(Natural.one)(Natural.successor))
    })

  /**
   * Constructs a set from the specified `Set`. The measure of how many times
   * a value occurs in the set will be a boolean representing whether a value
   * occurs at all.
   */
  def fromSet[A](set: Set[A]): ZSet[A, Boolean] =
    new ZSet(set.foldLeft(HashMap.empty[A, Boolean])((map, a) => map + (a -> true)))

  /**
   * Constructs a set from the specified `Map`. The elements will be the keys
   * of the `Map` and the measure of how many times an element occurs will be
   * the value for that key.
   */
  def fromMap[A, B](map: Map[A, B]): ZSet[A, B] =
    new ZSet(
      map match {
        case map: HashMap[A, B] => map
        case _                  => map.foldLeft(HashMap.empty[A, B])(_ + _)
      }
    )

  /**
   * The `ForEach` instance for `MultiSet`.
   */
  implicit lazy val MultiSetForEach: ForEach[MultiSet] =
    new ForEach[MultiSet] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: MultiSet[A])(f: A => G[B]): G[MultiSet[B]] =
        fa.forEach(f)
    }

  /**
   * Derives a `Commutative[ZSet[A, B]]` given a `Commutative[B]`.
   */
  implicit def ZSetCommutative[A, B](implicit
    ev: Commutative[B]
  ): Commutative[ZSet[A, B]] with Identity[ZSet[A, B]] =
    new Commutative[ZSet[A, B]] with Identity[ZSet[A, B]] {
      override def combine(l: => ZSet[A, B], r: => ZSet[A, B]): ZSet[A, B] = ZSetIdentity(ev).combine(l, r)

      override def identity: ZSet[A, B] = ZSetIdentity(ev).identity
    }

  /**
   * Derives a `Debug[ZSet[A, B]]` given a `Debug[A]` and `Debug[B]`.
   */
  implicit def ZSetDebug[A: Debug, B: Debug]: Debug[ZSet[A, B]] =
    chunk => Debug.Repr.VConstructor(List("zio", "prelude"), "ZSet", chunk.toMap.toList.map((t: (A, B)) => t.debug))

  /**
   * Derives an `Equal[ZSet[A, B]]` given an `Equal[B]`. Entries whose count
   * equals the identity of `Sum[B]` (zero for numbers) are ignored. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZSetEqual[A, B](implicit ev1: Equal[B], ev: Identity[Sum[B]]): Equal[ZSet[A, B]] =
    Equal[HashMap[A, B]].contramap(_.map.filterNot(_._2 === ev.identity))

  /**
   * The `DeriveEqual` instance for `ZSet`.
   */
  implicit def ZSetDeriveEqual[B: Equal](implicit
    ev: Identity[Sum[B]]
  ): DeriveEqual[({ type lambda[+x] = ZSet[x, B] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = ZSet[x, B] })#lambda] {
      def derive[A: Equal]: Equal[ZSet[A, B]] =
        ZSetEqual
    }

  /**
   * The `Covariant` instance for `ZSet`.
   */
  implicit def ZSetCovariant[B](implicit
    ev: Commutative[Sum[B]]
  ): Covariant[({ type lambda[+x] = ZSet[x, B] })#lambda] =
    new Covariant[({ type lambda[+x] = ZSet[x, B] })#lambda] {
      def map[A, C](f: A => C): ZSet[A, B] => ZSet[C, B] =
        _.map(f)
    }

  /**
   * Derives an `Idempotent[ZSet[A, B]]` given an `Idempotent[B]`.
   */
  implicit def ZSetIdempotent[A, B](implicit ev: Idempotent[B]): Idempotent[ZSet[A, B]] with Identity[ZSet[A, B]] =
    new Idempotent[ZSet[A, B]] with Identity[ZSet[A, B]] {
      override def combine(l: => ZSet[A, B], r: => ZSet[A, B]): ZSet[A, B] = ZSetIdentity(ev).combine(l, r)

      override def identity: ZSet[A, B] = ZSetIdentity(ev).identity
    }

  /**
   * Derives an `Identity[ZSet[A, B]]` given an `Associative[B]`. It combines
   * the counts of each element with the `Associative[B]` and uses the empty
   * set as the identity.
   */
  implicit def ZSetIdentity[A, B](implicit ev: Associative[B]): Identity[ZSet[A, B]] =
    new Identity[ZSet[A, B]] {
      override def combine(left: => ZSet[A, B], right: => ZSet[A, B]): ZSet[A, B] =
        new ZSet(right.map.foldLeft(left.map) { case (map, (a, b1)) =>
          map.get(a) match {
            case Some(b) => map + (a -> (b <> b1))
            case None    => map + (a -> b1)
          }
        })

      override def identity: ZSet[A, B] = ZSet.empty
    }

  /**
   * The `IdentityFlatten` instance for `ZSet`. Its `any` is the empty set,
   * so flattening `any.map(_ => fa)` gives the empty set rather than `fa`.
   */
  implicit def ZSetIdentityFlatten[B](implicit
    ev1: Commutative[Sum[B]],
    ev2: Commutative[Prod[B]]
  ): IdentityFlatten[({ type lambda[+x] = ZSet[x, B] })#lambda] =
    new IdentityFlatten[({ type lambda[+x] = ZSet[x, B] })#lambda] {
      def flatten[A](ffa: ZSet[ZSet[A, B], B]): ZSet[A, B] =
        ffa.flatMap(identity)
      def any: ZSet[Any, B]                                = ZSet.fromMap(Map.empty)

    }

  /**
   * Derives a `Hash[ZSet[A, B]]` given a `Hash[B]`. Entries whose count
   * equals the identity of `Sum[B]` are ignored. Due to the limitations of
   * Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZSetHash[A, B: Hash](implicit ev: Identity[Sum[B]]): Hash[ZSet[A, B]] =
    Hash[HashMap[A, B]].contramap(_.map.filterNot(_._2 === ev.identity))

}

/**
 * Lower-priority implicit instances for [[ZSet]].
 */
trait LowPriorityZSetImplicits {

  /**
   * Derives a `PartialOrd[ZSet[A, B]]` given a `PartialOrd[B]`.
   * Due to the limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZSetPartialOrd[A, B: PartialOrd](implicit ev: Identity[Sum[B]]): PartialOrd[ZSet[A, B]] =
    PartialOrd.makeFrom(
      (
        l,
        r
      ) => PartialOrd.compareSoft(l.toMap.filterNot(_._2 === ev.identity), r.toMap.filterNot(_._2 === ev.identity)),
      ZSet.ZSetEqual
    )

}

/**
 * Extension methods for building and taking apart `MultiSet` values.
 */
trait ZSetSyntax {

  /** Adds `toMultiSet` to a `Map[A, Natural]`. */
  implicit final class ZSetMapOps[+A](self: Map[A, Natural]) {

    /** Converts a `Map[A, Natural]` to a `MultiSet`, using each value as the count of its key. */
    def toMultiSet: MultiSet[A] = MultiSet.fromMap(self)
  }

  /** Adds `head`, `peel` and `tail` to a `MultiSet`. */
  implicit final class ZSetMultiSetOps[+A](self: MultiSet[A]) {

    private def headFiltered: Option[(A, Natural)] = self.toMap[A].find { case (_, n) => n > 0 }

    /** Returns an element with a count above zero, or `None` if there is none. */
    def head: Option[A] = headFiltered.map(_._1)

    /**
     * Returns an element of this `MultiSet` and the remainder, which is a (possibly empty) `MultiSet`
     * with one occurrence of that element removed, or `None` if no element has a count above zero.
     */
    def peel: Option[(A, MultiSet[A])] =
      headFiltered.map {
        case (k, v) if v > 1 =>
          (k, ZSet.fromMap(self.toMap + (k -> Natural.unsafeMake(v - 1))))
        case (k, _)          =>
          (k, ZSet.fromMap(self.toMap - k))
      }

    /**
     * Returns the tail of this `MultiSet` if it exists or `None` otherwise.
     */
    def tail: Option[MultiSet[A]] =
      peel.map(_._2)
  }
}
