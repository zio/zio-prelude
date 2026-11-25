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

import zio._
import zio.prelude.coherent.HashPartialOrd

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * A `NonEmptySet[A]` is an immutable `Set` that holds at least one element.
 * Operations that keep at least one element, such as `+`, `map` and `union`,
 * return a `NonEmptySet`; operations that can remove every element, such as
 * `filter` and `remove`, return a `Set`. An implicit conversion to `Set`
 * gives access to the other `Set` operations.
 *
 * Like `Set`, a `NonEmptySet` has no defined order: `head`, `peel` and
 * iteration follow the iteration order of the underlying `Set`.
 *
 * {{{
 * val tags: NonEmptySet[String] = NonEmptySet("scala", "zio")
 *
 * (tags + "prelude").size          // 3
 * tags.map(_.length).toSet         // Set(3, 5)
 * tags.remove("zio").contains("zio") // false
 * }}}
 */
final class NonEmptySet[A] private (private val set: Set[A]) { self =>

  /**
   * Creates a new `NonEmptySet` with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new set that contains all elements of this set and that also
   *          contains `elem`.
   */
  def +(elem: A): NonEmptySet[A] = new NonEmptySet(set + elem)

  /**
   * Creates a new `NonEmptySet` by adding all elements contained in another collection to this `NonEmptySet`, omitting duplicates.
   *
   * This method takes a collection of elements and adds all elements, omitting duplicates, into `NonEmptySet`.
   *
   * Example:
   *  {{{
   *    val a = NonEmptySet(1, 2) ++ List(2, 3)
   *    a.toSet == Set(1, 2, 3) // true
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptySet` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[A]): NonEmptySet[A] = new NonEmptySet(set ++ elems)

  /** Adds the `elem` to this `NonEmptySet`. Alias for `+`. */
  def add(elem: A): NonEmptySet[A] = self + elem

  /**
   * Returns a `Set` of the results of `pf` for the elements on which it is
   * defined.
   */
  def collect[B](pf: PartialFunction[A, B]): Set[B] =
    set.collect(pf)

  /**
   * Returns whether `that` is a `NonEmptySet` with the same elements. A
   * `NonEmptySet` is never equal to a `Set`.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySet[_]          => self.set == that.toSet
      case _                             => false
    }

  /** Returns whether at least one element satisfies the predicate `p`. */
  def exists(p: A => Boolean): Boolean =
    set.exists(p)

  /** Returns the elements that satisfy the predicate `p`, as a `Set`. */
  def filter(p: A => Boolean): Set[A] =
    set.filter(p)

  /** Returns the elements that do not satisfy the predicate `p`, as a `Set`. */
  def filterNot(p: A => Boolean): Set[A] =
    set.filterNot(p).asInstanceOf[Set[A]]

  /**
   * Returns the first element, in iteration order, that satisfies the
   * predicate `p`, or `None` if no element does.
   */
  def find(p: A => Boolean): Option[A] =
    set.find(p)

  /**
   * Flattens a `NonEmptySet` of `NonEmptySet` values into a single
   * `NonEmptySet`.
   */
  def flatten[B](implicit ev: A <:< NonEmptySet[B]): NonEmptySet[B] =
    new NonEmptySet[B](set.foldLeft[Set[B]](Set.empty)((b, a) => b union ev(a)))

  /**
   * Folds over the elements in iteration order, starting from `z` and
   * combining with `op`.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    set.foldLeft(z)(op)

  /** Returns whether every element satisfies the predicate `p`. */
  def forall(p: A => Boolean): Boolean =
    set.forall(p)

  /**
   * Splits the elements, in iteration order, into `NonEmptySet`s of `size`
   * elements each; the last one may be smaller. Throws a
   * `java.lang.IllegalArgumentException` when `size` is zero or negative.
   */
  def grouped[B >: A](size: Int): Iterator[NonEmptySet[B]] =
    set.grouped(size).map(new NonEmptySet(_)).asInstanceOf[Iterator[NonEmptySet[B]]]

  /** Returns a hash code computed from the underlying `Set`. */
  override def hashCode: Int = set.hashCode ^ NonEmptySet.NonEmptySetSeed

  /** Returns the first element in iteration order. */
  def head: A =
    set.head

  /** Returns every element except the last one in iteration order, as a `Set`. */
  def init: Set[A] =
    set.init

  /** Returns an `Iterator` over the elements. */
  def iterator: Iterator[A] =
    set.iterator

  /** Returns the last element in iteration order. */
  def last: A =
    set.last

  /**
   * Returns a `NonEmptySet` of the results of applying `f` to each element.
   * Results that are equal are kept once, so the result can be smaller than
   * this set.
   */
  def map[B](f: A => B): NonEmptySet[B] = new NonEmptySet(set.map(f))

  /**
   * Returns an element of this `NonEmptySet` and the remainder, which is a (possibly empty) `Set`.
   * The element is the first one in iteration order.
   */
  @inline
  def peel: (A, Set[A]) = (set.head, set.tail)

  /**
   * Returns an element of this `NonEmptySet`
   * and the remainder or `None`, if the remainder is empty.
   */
  def peelNonEmpty: (A, Option[NonEmptySet[A]]) = {
    val (head, tail) = peel
    if (tail.isEmpty)
      (head, None)
    else
      (head, Some(new NonEmptySet(tail)))
  }

  /**
   * Combines the elements with `op`, in iteration order. For a single
   * element, returns that element.
   */
  def reduce[B >: A](op: (B, B) => B): B =
    set.reduce(op)

  /**
   * Returns the elements of this `NonEmptySet` other than `elem`, as a `Set`.
   * The result is empty when `elem` is the only element.
   */
  def remove(elem: A): Set[A] = set - elem

  /**
   * Removes `elem` from this `NonEmptySet`, returning the rest as a
   * `NonEmptySet`, or `None` if nothing is left.
   */
  def removeNonEmpty(elem: A): Option[NonEmptySet[A]] = {
    val newSet = set - elem
    if (newSet.nonEmpty) Some(new NonEmptySet(newSet)) else None
  }

  /** Returns every element except the first one in iteration order, as a `Set`. */
  def tail: Set[A] =
    set.tail

  /**
   * Returns the elements except the first one in iteration order, as a
   * `NonEmptySet`, or `None` if there is only one element.
   */
  def tailNonEmpty: Option[NonEmptySet[A]] = peelNonEmpty._2

  /** Copies the elements, in iteration order, into a new `Array`. */
  def toArray[B >: A: ClassTag]: Array[B] =
    set.toArray

  /** Returns the elements as an `Iterable`, which is the underlying `Set`. */
  def toIterable: Iterable[A] =
    set

  /** Returns the elements, in iteration order, as a `List`. */
  def toList: List[A] =
    set.toList

  /**
   * Converts this `NonEmptySet` to a `NonEmptyChunk`.
   */
  def toNonEmptyChunk: NonEmptyChunk[A] = peel match { case (head, tail) => NonEmptyChunk.fromIterable(head, tail) }

  /**
   * Converts this `NonEmptySet` to a `NonEmptyList`.
   */
  def toNonEmptyList: NonEmptyList[A] = peel match { case (head, tail) => NonEmptyList.fromIterable(head, tail) }

  /** Converts this `NonEmptySet` to a `Set`. */
  def toSet: Set[A] = set

  /** Renders this `NonEmptySet` as `NonEmpty` followed by the `toString` of the underlying `Set`. */
  override def toString: String = s"NonEmpty$set"

  /**
   * Computes the union of this `NonEmptySet` and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new `NonEmptySet` consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: Set[A]): NonEmptySet[A] = new NonEmptySet(set.union(that))

  /**
   * Pairs the elements of this set with the elements of `that`, both in
   * iteration order, and stops at the end of the smaller set. Which elements
   * are paired depends on the iteration order of each set.
   */
  def zip[B](that: NonEmptySet[B])(implicit zippable: Zippable[A, B]): NonEmptySet[zippable.Out] =
    new NonEmptySet(self.set.zip(that.set).map { case (a, b) => zippable.zip(a, b) })

  /** Pairs each element with its position, from `0`, in iteration order. */
  def zipWithIndex[B >: A]: NonEmptySet[(B, Int)] =
    new NonEmptySet(set.zipWithIndex.map { case (a, i) => (a, i) })
}

/**
 * Constructors and type class instances for `NonEmptySet`.
 */
object NonEmptySet {

  /**
   * The `Commutative` and `Idempotent` (and thus `Associative`) instance for `NonEmptySet`,
   * which combines two sets by union.
   */
  implicit def NonEmptySetCommutativeIdempotent[A]: Commutative[NonEmptySet[A]] with Idempotent[NonEmptySet[A]] =
    new Commutative[NonEmptySet[A]] with Idempotent[NonEmptySet[A]] {
      override def combine(l: => NonEmptySet[A], r: => NonEmptySet[A]): NonEmptySet[A] = l union r
    }

  /**
   * Derives a `Debug[NonEmptySet[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptySetDebug[A: Debug]: Debug[NonEmptySet[A]] =
    chunk => Debug.Repr.VConstructor(List("zio", "prelude"), "NonEmptySet", chunk.toNonEmptyList.map(_.debug).toCons)

  /**
   * Derives a `Hash[NonEmptySet[A]]` and `PartialOrd[NonEmptySet[A]]` (and thus `Equal[NonEmptySet[A]]`) instance.
   * It needs no instance for `A`: elements are compared with `==` and hashed
   * with `hashCode`. The partial order is set inclusion, so
   * `NonEmptySet(1)` is less than `NonEmptySet(1, 2)` and incomparable with
   * `NonEmptySet(2)`.
   */
  implicit def NonEmptySetHashPartialOrd[A]: Hash[NonEmptySet[A]] with PartialOrd[NonEmptySet[A]] =
    HashPartialOrd.derive[Set[A]].contramap(_.toSet)

  /**
   * Provides an implicit conversion from `NonEmptySet` to the `Set`
   * for interoperability with Scala's collection library.
   */
  implicit def toSet[A](nonEmptySet: NonEmptySet[A]): Set[A] =
    nonEmptySet.toSet

  /**
   * The `CommutativeEither` instance for `NonEmptySet`. `either` returns the
   * union of the first set's elements wrapped in `Left` and the second set's
   * elements wrapped in `Right`.
   */
  implicit val NonEmptySetCommutativeEither: CommutativeEither[NonEmptySet] =
    new CommutativeEither[NonEmptySet] {
      def either[A, B](fa: => NonEmptySet[A], fb: => NonEmptySet[B]): NonEmptySet[Either[A, B]] =
        fa.map[Either[A, B]](Left(_)).union(fb.map[Either[A, B]](Right(_)))
    }

  /**
   * The `DeriveEqual` instance for `NonEmptySet`. The derived `Equal` does not
   * use the `Equal[A]` it is given: it compares the sets with `==`.
   */
  implicit val NonEmptySetDeriveEqual: DeriveEqual[NonEmptySet] =
    new DeriveEqual[NonEmptySet] {
      def derive[A: Equal]: Equal[NonEmptySet[A]] =
        NonEmptySetHashPartialOrd
    }

  /**
   * The `Invariant` instance for `NonEmptySet`.
   */
  implicit val NonEmptySetInvariant: Invariant[NonEmptySet] =
    new Invariant[NonEmptySet] {
      def invmap[A, B](f: A <=> B): NonEmptySet[A] <=> NonEmptySet[B] =
        Equivalence[NonEmptySet[A], NonEmptySet[B]](a => a.map(f.to), b => b.map(f.from))
    }
  private val NonEmptySetSeed: Int                          = 1247820194

  /**
   * Creates a `NonEmptySet` with the specified elements.
   *  @tparam A      the type of the `NonEmptySet`'s elements
   *  @param elem    an element of the created `NonEmptySet`
   *  @param others  the remaining elements of the created `NonEmptySet`
   *  @return a new `NonEmptySet` with elements `elem` and `others`
   */
  def apply[A](elem: A, others: A*): NonEmptySet[A] = apply(elem, others.toSet)

  /**
   * Extracts the element and the remainder that `peel` returns, so that
   * `case NonEmptySet(elem, rest) => ...` matches every `NonEmptySet`.
   */
  def unapply[A](arg: NonEmptySet[A]): Some[(A, Set[A])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptySet` from a `NonEmptyChunk`.
   */
  def fromNonEmptyChunk[A](elems: NonEmptyChunk[A]): NonEmptySet[A] = apply(elems.head, elems.tail.toSet)

  /**
   * Constructs a `NonEmptySet` from a `NonEmptyList`.
   */
  def fromNonEmptyList[A](elems: NonEmptyList[A]): NonEmptySet[A] = apply(elems.head, elems.tail.toSet)

  /**
   * Constructs a `NonEmptySet` from an element and `Set`.
   */
  def fromSet[A](elem: A, others: Set[A]): NonEmptySet[A] = apply(elem, others)

  /**
   * Constructs a `NonEmptySet` from a `Set`, or returns `None` when the `Set`
   * is empty.
   */
  def fromSetOption[A](set: Set[A]): Option[NonEmptySet[A]] = set.headOption.map(fromSet(_, set.tail))

  /**
   * Constructs a `NonEmptySet` from an element and `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptySet[A] =
    fromSet(head, tail.toSet)

  /**
   * Constructs a `NonEmptySet` from an `Iterable`, or returns `None` when the
   * `Iterable` is empty.
   */
  def fromIterableOption[A](iterable: Iterable[A]): Option[NonEmptySet[A]] =
    iterable.headOption.fold(None: Option[NonEmptySet[A]])(h => Some(fromIterable(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySet` with the specified single value.
   */
  def single[A](head: A): NonEmptySet[A] =
    NonEmptySet(head)

  /** Creates a `NonEmptySet` containing elements from `l` and `r`. */
  def union[A](l: NonEmptySet[A], r: Set[A]): NonEmptySet[A] = {
    val (head, tail) = l.peel
    NonEmptySet.fromSet(head, tail.union(r))
  }

  /** Creates a `NonEmptySet` containing elements from `l` and `r`. */
  def union[A](l: Set[A], r: NonEmptySet[A]): NonEmptySet[A] =
    union(r, l)

  private def apply[A](elem: A, others: Set[A]): NonEmptySet[A] = new NonEmptySet(others + elem)
}

/**
 * Extension methods that convert Scala collections to `NonEmptySet`. The
 * `zio.prelude` package object mixes this trait in, so
 * `import zio.prelude._` brings them into scope.
 */
trait NonEmptySetSyntax {

  /** Adds `toNonEmptySet` to every `Iterable`. */
  implicit final class NonEmptySetIterableOps[A](private val iterable: Iterable[A]) {

    /**
     * Constructs a `NonEmptySet` from an `Iterable`, or returns `None` when
     * the `Iterable` is empty.
     */
    def toNonEmptySet: Option[NonEmptySet[A]] = NonEmptySet.fromIterableOption(iterable)
  }

  /** Adds `toNonEmptySet` to every `Set`. */
  implicit final class NonEmptySetSetOps[A](self: Set[A]) {

    /**
     * Constructs a `NonEmptySet` from a `Set`, or returns `None` when the
     * `Set` is empty.
     */
    def toNonEmptySet: Option[NonEmptySet[A]] = NonEmptySet.fromSetOption(self)
  }
}
