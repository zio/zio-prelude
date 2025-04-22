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
   *    scala> val a = NonEmptySet(1, 2) ++ NonEmptySet(2, "a")
   *    a: zio.prelude.NonEmptySet[Any] = NonEmptySet(1, 2, a)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new `NonEmptySet` with the given elements added, omitting duplicates.
   */
  def ++(elems: Iterable[A]): NonEmptySet[A] = new NonEmptySet(set ++ elems)

  /** Adds the `elem` to this `NonEmptySet`. Alias for `+`. */
  def add(elem: A): NonEmptySet[A] = self + elem

  def collect[B](pf: PartialFunction[A, B]): Set[B] =
    set.collect(pf)

  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySet[_]          => self.set == that.toSet
      case _                             => false
    }

  def exists(p: A => Boolean): Boolean =
    set.exists(p)

  def filter(p: A => Boolean): Set[A] =
    set.filter(p)

  def filterNot(p: A => Boolean): Set[A] =
    set.filterNot(p).asInstanceOf[Set[A]]

  def find(p: A => Boolean): Option[A] =
    set.find(p)

  /**
   * Flattens a `NonEmptySet` of `NonEmptySet` values into a single
   * `NonEmptySet`.
   */
  def flatten[B](implicit ev: A <:< NonEmptySet[B]): NonEmptySet[B] =
    new NonEmptySet[B](set.foldLeft[Set[B]](Set.empty)((b, a) => b union ev(a)))

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    set.foldLeft(z)(op)

  def forall(p: A => Boolean): Boolean =
    set.forall(p)

  def grouped[B >: A](size: Int): Iterator[NonEmptySet[B]] =
    set.grouped(size).map(new NonEmptySet(_)).asInstanceOf[Iterator[NonEmptySet[B]]]

  override def hashCode: Int = set.hashCode ^ NonEmptySet.NonEmptySetSeed

  def head: A =
    set.head

  def init: Set[A] =
    set.init

  def iterator: Iterator[A] =
    set.iterator

  def last: A =
    set.last

  def map[B](f: A => B): NonEmptySet[B] = new NonEmptySet(set.map(f))

  /**
   * Returns an element of this `NonEmptySet` and the remainder, which is a (possibly empty) `Set`.
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

  def reduce[B >: A](op: (B, B) => B): B =
    set.reduce(op)

  /** Removes the `elem` from this `NonEmptySet`. Alias for `-`. */
  def remove(elem: A): Set[A] = set - elem

  /**
   * removes the elem from  `NonEmptySet`, returning Some(NonEmptySet) if there's anything
   * left, otherwise None
   */
  def removeNonEmpty(elem: A): Option[NonEmptySet[A]] = {
    val newSet = set - elem
    if (newSet.nonEmpty) Some(new NonEmptySet(newSet)) else None
  }

  def tail: Set[A] =
    set.tail

  /**
   * Returns the tail of this `NonEmptySet` if it exists or `None` otherwise.
   */
  def tailNonEmpty: Option[NonEmptySet[A]] = peelNonEmpty._2

  def toArray[B >: A: ClassTag]: Array[B] =
    set.toArray

  def toIterable: Iterable[A] =
    set

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

  override def toString: String = s"NonEmpty$set"

  /**
   * Computes the union between of `NonEmptySet` and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new `NonEmptySet` consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: Set[A]): NonEmptySet[A] = new NonEmptySet(set.union(that))

  def zip[B](that: NonEmptySet[B])(implicit zippable: Zippable[A, B]): NonEmptySet[zippable.Out] =
    new NonEmptySet(self.set.zip(that.set).map { case (a, b) => zippable.zip(a, b) })

  def zipWithIndex[B >: A]: NonEmptySet[(B, Int)] =
    new NonEmptySet(set.zipWithIndex.map { case (a, i) => (a, i) })
}

object NonEmptySet {

  /**
   * The `Commutative` and `Idempotent` (and thus `Associative`) instance for `NonEmptySet`.
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
   * Derives a `Hash[NonEmptySet[A]]` and `PartialOrd[NonEmptySet[A]]` (and thus `Equal[NonEmptyList[A]]`) instance.
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
   * The `CommutativeEither` instance for `NonEmptySet`.
   */
  implicit val NonEmptySetCommutativeEither: CommutativeEither[NonEmptySet] =
    new CommutativeEither[NonEmptySet] {
      def either[A, B](fa: => NonEmptySet[A], fb: => NonEmptySet[B]): NonEmptySet[Either[A, B]] =
        fa.map[Either[A, B]](Left(_)).union(fb.map[Either[A, B]](Right(_)))
    }

  /**
   * The `DeriveEqual` instance for `NonEmptySet`.
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

  def unapply[A](arg: NonEmptySet[A]): Some[(A, Set[A])] = Some(arg.peel)

  /**
   * Constructs a `NonEmptyChunk` from a `NonEmptyList`.
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
   * Constructs a `NonEmptySet` from a `Set` or `None` otherwise.
   */
  def fromSetOption[A](set: Set[A]): Option[NonEmptySet[A]] = set.headOption.map(fromSet(_, set.tail))

  /**
   * Constructs a `NonEmptySet` from an element and `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptySet[A] =
    fromSet(head, tail.toSet)

  /**
   * Constructs a `NonEmptySet` from an `Iterable` or `None` otherwise.
   */
  def fromIterableOption[A](iterable: Iterable[A]): Option[NonEmptySet[A]] =
    iterable.headOption.fold(None: Option[NonEmptySet[A]])(h => Some(fromIterable(h, iterable.tail)))

  /**
   * Constructs a `NonEmptySet` with the specified single value.
   */
  def single[A](head: A): NonEmptySet[A] =
    NonEmptySet(head)

  /** Creates a `NonEmptySet` containing elements from `l` and `r` */
  def union[A](l: NonEmptySet[A], r: Set[A]): NonEmptySet[A] = {
    val (head, tail) = l.peel
    NonEmptySet.fromSet(head, tail.union(r))
  }

  /** Creates a `NonEmptySet` containing elements from `l` and `r` */
  def union[A](l: Set[A], r: NonEmptySet[A]): NonEmptySet[A] =
    union(r, l)

  private def apply[A](elem: A, others: Set[A]): NonEmptySet[A] = new NonEmptySet(others + elem)
}

trait NonEmptySetSyntax {
  implicit final class NonEmptySetIterableOps[A](private val iterable: Iterable[A]) {

    /**
     * Constructs a `NonEmptySet` from an `Iterable` or `None` otherwise.
     */
    def toNonEmptySet: Option[NonEmptySet[A]] = NonEmptySet.fromIterableOption(iterable)
  }
  implicit final class NonEmptySetSetOps[A](self: Set[A]) {

    /**
     * Constructs a `NonEmptySet` from a `Set` or `None` otherwise.
     */
    def toNonEmptySet: Option[NonEmptySet[A]] = NonEmptySet.fromSetOption(self)
  }
}
