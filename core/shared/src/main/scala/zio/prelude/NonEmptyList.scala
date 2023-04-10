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
import zio.prelude.NonEmptyList._
import zio.prelude.newtypes.{Max, Min, Prod, Sum}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.hashing.MurmurHash3

/**
 * A `NonEmptyList[A]` is a list of one or more values of type A. Unlike a
 * `List`, a `NonEmptyList` is guaranteed to contain at least one element.
 * This additional structure allows some operations to be defined on
 * `NonEmptyList` that are not safe on `List`, such as `head` and `reduceAll`.
 *
 * For interoperability with Scala's collection library an implicit conversion
 * is provided from `NonEmptyList` to the `::` case of `List`. Operations that
 * cannot preserve the guarantee that the resulting collection must have at
 * least one element will return a `List` instead.
 */
sealed trait NonEmptyList[+A] { self =>

  /**
   * Concatenates this `NonEmptyList` with the specified `NonEmptyList`.
   */
  final def ++[A1 >: A](that: NonEmptyList[A1]): NonEmptyList[A1] =
    foldRight(that)(cons)

  /**
   * Concatenates this `NonEmptyList` with the specified `Iterable`.
   */
  final def ++[A1 >: A](that: Iterable[A1]): NonEmptyList[A1] =
    NonEmptyList.fromIterableOption(that).fold[NonEmptyList[A1]](self)(self ++ _)

  /**
   * Prepends the specified value to this `NonEmptyList`.
   */
  final def ::[A1 >: A](a: A1): NonEmptyList[A1] =
    cons(a, self)

  /**
   * Returns whether this `NonEmptyList` contains the specified element.
   */
  final def contains[A1 >: A](a: A1)(implicit A: Equal[A1]): Boolean =
    exists(_ === a)

  /**
   * Determines whether this `NonEmptyList` and the specified `NonEmptyList`
   * have the same length and every pair of corresponding elements of this
   * `NonEmptyList` and the specified `NonEmptyList` satisfy the specified
   * predicate.
   */
  @tailrec
  final def corresponds[B](that: NonEmptyList[B])(f: (A, B) => Boolean): Boolean =
    (self, that) match {
      case (Cons(h1, t1), Cons(h2, t2)) if f(h1, h2) => t1.corresponds(t2)(f)
      case (Single(h1), Single(h2))                  => f(h1, h2)
      case _                                         => false
    }

  /**
   * Returns the number of elements in this `NonEmptyList` that satisfy the
   * specified predicate.
   */
  final def count(f: A => Boolean): Int =
    foldLeft(0)((n, a) => if (f(a)) n + 1 else n)

  /**
   * Removes duplicate elements from this `NonEmptyList`.
   */
  final def distinct: NonEmptyList[A] =
    reduceMapLeft(a => (single(a), Set(a))) { case ((as, seen), a) =>
      if (seen(a)) (as, seen) else (cons(a, as), seen + a)
    }._1.reverse

  /** Decomposes the `NonEmptyList` into an element and a (possibly empty) `List` */
  final def peel: (A, List[A]) = self match {
    case Single(head)     => (head, List())
    case Cons(head, tail) => (head, tail.toList)
  }

  /**
   * Returns an element of this `NonEmptyList`
   * and the remainder or `None`, if the remainder is empty.
   */
  final def peelNonEmpty: (A, Option[NonEmptyList[A]]) = self match {
    case Single(head)     => (head, None)
    case Cons(head, tail) => (head, Some(tail))
  }

  /**
   * Drops the first `n` elements from this `NonEmptyList` returning a `List`.
   */
  @tailrec
  final def drop(n: Int): List[A] =
    if (n <= 0) self
    else
      self match {
        case Cons(_, t) => t.drop(n - 1)
        case Single(_)  => Nil
      }

  /**
   * Drops the last `n` elements from this `NonEmptyList` returning a `List`.
   */
  final def dropRight(n: Int): List[A] =
    take(length - n)

  /**
   * Drops elements from the start of this `NonEmptyList` that satisfy the
   * specified predicate returning a `List`.
   */
  @tailrec
  final def dropWhile(f: A => Boolean): List[A] =
    self match {
      case Cons(h, t) => if (f(h)) t.dropWhile(f) else h :: t
      case Single(h)  => if (f(h)) Nil else single(h)
    }

  /**
   * Returns whether this `NonEmptyList` and the specified `NonEmptyList` are
   * equal to each other.
   */
  override final def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptyList[_]         => self.corresponds(that)(_ == _)
      case _                             => false
    }

  /**
   * Returns whether an element exists in this `NonEmptyList` satisfying the
   * specified predicate.
   */
  @tailrec
  final def exists(f: A => Boolean): Boolean =
    self match {
      case Cons(h, t) => if (f(h)) true else t.exists(f)
      case Single(h)  => f(h)
    }

  /**
   * Returns the first element in this `NonEmptyList` satisfying the
   * specified predicate or `None` otherwise.
   */
  @tailrec
  final def find(f: A => Boolean): Option[A] =
    self match {
      case Cons(h, t) => if (f(h)) Some(h) else t.find(f)
      case Single(h)  => if (f(h)) Some(h) else None
    }

  /**
   * Transforms each element of this `NonEmptyList` to a `NonEmptyList` and
   * combines them into a single `NonEmptyList`.
   */
  final def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    reduceMapRight(f)((a, bs) => f(a) ++ bs)

  /**
   * Flattens a `NonEmptyList` of `NonEmptyList` values into a single
   * `NonEmptyList`.
   */
  final def flatten[B](implicit ev: A <:< NonEmptyList[B]): NonEmptyList[B] =
    flatMap(ev)

  /**
   * Folds over the elements of this `NonEmptyList` from left to right using
   * the specified initial value and combining function
   */
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    self match {
      case Cons(h, t) => t.foldLeft(f(z, h))(f)
      case Single(h)  => f(z, h)
    }

  /**
   * Folds over the elements of this `NonEmptyList` from right to left using
   * the specified initial value and combining function.
   */
  final def foldRight[B](z: B)(op: (A, B) => B): B =
    self.reverse.foldLeft(z)((b, a) => op(a, b))

  /**
   * Returns whether all elements of this `NonEmptyList` satisfy the specified
   * predicate.
   */
  @tailrec
  final def forall(f: A => Boolean): Boolean =
    self match {
      case Cons(h, t) => if (f(h)) t.forall(f) else false
      case Single(h)  => f(h)
    }

  /**
   * Transforms each element of this `NonEmptyList` with the specified
   * effectual function.
   */
  final def forEach[F[+_]: AssociativeBoth: Covariant, B](f: A => F[B]): F[NonEmptyList[B]] =
    reduceMapRight(f(_).map(single))((a, fas) => f(a).zipWith(fas)(cons))

  /**
   * Returns the hashCode of this `NonEmptyList`.
   */
  override final def hashCode: Int = {
    val (hash, n) = foldLeft((nonEmptyListSeed, 0)) { case ((hash, n), a) =>
      (MurmurHash3.mix(hash, a.hashCode), n + 1)
    }
    MurmurHash3.finalizeHash(hash, n)
  }

  /**
   * Returns the head of this `NonEmptyList`.
   */
  def head: A

  /**
   * Returns the length of this `NonEmptyList`.
   */
  final def length: Int =
    foldLeft(0)((n, _) => n + 1)

  /**
   * Transforms the elements of this `NonEmptyList` with the specified
   * function.
   */
  final def map[B](f: A => B): NonEmptyList[B] =
    reduceMapRight(a => single(f(a)))((a, bs) => cons(f(a), bs))

  /**
   * Returns the maximum element in this `NonEmptyList`.
   */
  final def max(implicit A: Ord[A]): A =
    maxBy(identity)

  /**
   * Returns the maximum element in this `NonEmptyList` using the specified
   * function to map values of type `A` to values of type `B` that an ordering
   * is defined on.
   */
  final def maxBy[B](f: A => B)(implicit B: Ord[B]): A = {
    implicit val A: Ord[A] = B.contramap(f)
    reduceMap(Max[A])
  }

  /**
   * Returns the minimum element in this `NonEmptyList`.
   */
  final def min(implicit A: Ord[A]): A =
    minBy(identity)

  /**
   * Returns the minimum element in this `NonEmptyList` using the specified
   * function to map values of type `A` to values of type `B` that an ordering
   * is defined on.
   */
  final def minBy[B](f: A => B)(implicit B: Ord[B]): A = {
    implicit val A: Ord[A] = B.contramap(f)
    reduceMap(Min[A])
  }

  /**
   * Renders the elements of this `NonEmptyList` as a `String`.
   */
  final def mkString: String =
    mkString("")

  /**
   * Renders the elements of this `NonEmptyList` as a `String` using the
   * specified separator.
   */
  final def mkString(sep: String): String =
    mkString("", sep, "")

  /**
   * Renders the elements of this `NonEmptyList` as a `String` using the
   * specified separator and start and end values.
   */
  final def mkString(start: String, sep: String, end: String): String =
    start + reduceMapLeft(_.toString)((b, a) => b + sep + a.toString) + end

  /**
   * Returns the product of the elements of this `NonEmptyList`.
   */
  final def product[A1 >: A](implicit A: Associative[Prod[A1]]): A1 =
    reduceMap(Prod[A1])

  /**
   * Reduces the elements of this `NonEmptyList` using the specified
   * associative operator.
   */
  final def reduce[A1 >: A](implicit A: Associative[A1]): A1 =
    reduceMap[A1](identity)

  /**
   * Reduces the elements of this `NonEmptyList` from left to right using the
   * specified function.
   */
  final def reduceLeft[A1 >: A](f: (A1, A1) => A1): A1 =
    reduceMapLeft[A1](identity)(f)

  /**
   * Maps each element of this `NonEmptyList` to a type `B` that has an
   * associative operation then combines them all with the associative
   * operation.
   */
  final def reduceMap[B](f: A => B)(implicit B: Associative[B]): B =
    reduceMapLeft(f)((b, a) => B.combine(b, f(a)))

  /**
   * Reduces the elements of this `NonEmptyList` from left to right using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduceAll` to combine the `B` value with each other `A` value.
   */
  final def reduceMapLeft[B](map: A => B)(reduce: (B, A) => B): B =
    self match {
      case Cons(h, t) => t.foldLeft(map(h))(reduce)
      case Single(t)  => map(t)
    }

  /**
   * Reduces the elements of this `NonEmptyList` from right to left using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduceAll` to combine the `B` value with each other `A` value.
   */
  final def reduceMapRight[B](map: A => B)(reduce: (A, B) => B): B =
    self.reverse.reduceMapLeft(map)((b, a) => reduce(a, b))

  /**
   * Reduces the elements of this `NonEmptyList` from right to left using the
   * specified function.
   */
  final def reduceRight[A1 >: A](f: (A1, A1) => A1): A1 =
    reduceMapRight[A1](identity)(f)

  /**
   * Reverses the order of elements in this `NonEmptyList`.
   */
  final def reverse: NonEmptyList[A] =
    reduceMapLeft(single)((b, a) => cons(a, b))

  /**
   * Returns the sum of the elements of this `NonEmptyList`.
   */
  final def sum[A1 >: A](implicit A: Associative[Sum[A1]]): A1 =
    reduceMap(Sum[A1])

  /**
   * Returns the tail of this `NonEmptyList` if it exists or `None` otherwise.
   */
  final def tailNonEmpty: Option[NonEmptyList[A]] =
    self match {
      case Cons(_, t) => Some(t)
      case _          => None
    }

  /**
   * Returns a new `NonEmptyList` composed of this `NonEmptyList` followed by
   * each of its tails, ending with a singleton `NonEmptyList`.
   */
  final def tails: NonEmptyList[NonEmptyList[A]] =
    unfold(self)(identity)(_.tailNonEmpty)

  /**
   * Takes the first `n` elements from this `NonEmptyList` returning a `List`.
   */
  final def take(n: Int): List[A] = {
    @tailrec
    def loop(n1: Int, nel: NonEmptyList[A], taken: List[A]): List[A] =
      if (n1 <= 0) taken
      else
        nel match {
          case Cons(h, t) => loop(n1 - 1, t, h :: taken)
          case Single(h)  => h :: taken
        }

    loop(n, self, Nil).reverse
  }

  /**
   * Takes the last `n` elements from this `NonEmptyList` returning a `List`.
   */
  final def takeRight(n: Int): List[A] =
    drop(length - n)

  /**
   * Takes elements from the start of this `NonEmptyList` that satisfy the
   * specified predicate returning a `List`.
   */
  final def takeWhile(f: A => Boolean): List[A] = {
    @tailrec
    def loop(nel: NonEmptyList[A], taken: List[A]): List[A] =
      nel match {
        case Cons(h, t) => if (f(h)) loop(t, h :: taken) else taken
        case Single(h)  => if (f(h)) h :: taken else taken
      }

    loop(self, Nil).reverse
  }

  /**
   * Converts this `NonEmptyList` to the `::` case of a `List`.
   */
  final def toCons[A1 >: A]: ::[A1] = {
    import scala.collection.immutable.{:: => cons}

    reduceMapRight[cons[A1]](cons(_, Nil))(cons(_, _))
  }

  /**
   * Renders this `NonEmptyList` as a `String`.
   */
  override final def toString: String =
    mkString("NonEmptyList(", ", ", ")")

  /**
   * Converts this `NonEmptyList` to a `NonEmptyChunk`.
   */
  final def toNonEmptyChunk: NonEmptyChunk[A] =
    NonEmptyChunk.fromCons(toCons)

  /**
   * Zips this `NonEmptyList` together with the specified `NonEmptyList`,
   * returning a new `NonEmptyList` with a length equal to the minimum of the
   * two and elements combined pairwise.
   */
  final def zip[B](that: NonEmptyList[B]): NonEmptyList[(A, B)] =
    zipWith(that)((_, _))

  /**
   * Zips this `NonEmptyList` together with the specified `NonEmptyList`,
   * returning a new `NonEmptyList` with a length equal to the minimum of the
   * two and elements combined pairwise using the specified function.
   */
  final def zipWith[B, C](that: NonEmptyList[B])(f: (A, B) => C): NonEmptyList[C] =
    unfold((self, that)) { case (l, r) =>
      f(l.head, r.head)
    } {
      case (Cons(_, t1), Cons(_, t2)) => Some((t1, t2))
      case _                          => None
    }

  /**
   * Annotates each element of this `NonEmptyList` with its index.
   */
  final def zipWithIndex: NonEmptyList[(A, Int)] =
    unfold((self, 0)) { case (as, n) =>
      (as.head, n)
    } {
      case (Cons(_, t), n) => Some((t, n + 1))
      case _               => None
    }
}

object NonEmptyList extends LowPriorityNonEmptyListImplicits {

  final case class Cons[+A](head: A, tail: NonEmptyList[A]) extends NonEmptyList[A]
  final case class Single[+A](head: A)                      extends NonEmptyList[A]

  /**
   * The `Associative` instance for `NonEmptyList`.
   */
  implicit def NonEmptyListAssociative[A]: Associative[NonEmptyList[A]] =
    Associative.make(_ ++ _)

  /**
   * The `AssociativeEither` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListAssociativeEither: AssociativeEither[NonEmptyList] =
    new AssociativeEither[NonEmptyList] {
      def either[A, B](as: => NonEmptyList[A], bs: => NonEmptyList[B]): NonEmptyList[Either[A, B]] =
        as.map(Left(_)) ++ bs.map(Right(_))
    }

  /**
   * The `IdentityFlatten` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListIdentityFlatten: IdentityFlatten[NonEmptyList] =
    new IdentityFlatten[NonEmptyList] {
      val any: NonEmptyList[Any]                                          = single(())
      def flatten[A](ffa: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] =
        ffa.flatten
    }

  /**
   * The `Covariant` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListCovariant: Covariant[NonEmptyList] =
    new Covariant[NonEmptyList] {
      def map[A, B](f: A => B): NonEmptyList[A] => NonEmptyList[B] =
        nonEmptyList => nonEmptyList.map(f)
    }

  /**
   * Derives a `Debug[NonEmptyList[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptyListDebug[A: Debug]: Debug[NonEmptyList[A]] =
    chunk => Debug.Repr.VConstructor(List("zio", "prelude"), "NonEmptyList", chunk.map(_.debug).toCons)

  /**
   * Derives an `Equal[NonEmptyList[A]]` given an `Equal[A]`.
   */
  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] =
    Equal.make(_.corresponds(_)(_ === _))

  /**
   * The `DeriveEqual` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListDeriveEqual: DeriveEqual[NonEmptyList] =
    new DeriveEqual[NonEmptyList] {
      def derive[A: Equal]: Equal[NonEmptyList[A]] =
        NonEmptyListEqual
    }

  /**
   * Derives a `Hash[NonEmptyList[A]]` given a `Hash[A]`.
   */
  implicit def NonEmptyListHash[A: Hash]: Hash[NonEmptyList[A]] =
    Hash.make(_.map(_.hash).hashCode, _.corresponds(_)(_ === _))

  /**
   * The `IdentityBoth` (and thus `AssociativeBoth`) instance for `NonEmptyList`.
   */
  implicit val NonEmptyListIdentityBoth: IdentityBoth[NonEmptyList] =
    new IdentityBoth[NonEmptyList] {
      val any: NonEmptyList[Any]                                                           =
        single(())
      def both[A, B](fa: => NonEmptyList[A], fb: => NonEmptyList[B]): NonEmptyList[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `NonEmptyForEach` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListNonEmptyForEach: NonEmptyForEach[NonEmptyList] =
    new NonEmptyForEach[NonEmptyList] {
      def forEach1[F[+_]: AssociativeBoth: Covariant, A, B](fa: NonEmptyList[A])(f: A => F[B]): F[NonEmptyList[B]] =
        fa.forEach(f)
    }

  /**
   * Constructs a `NonEmptyList` from one or more values.
   */
  def apply[A](head: A, tail: A*): NonEmptyList[A] =
    fromIterable(head, tail)

  /**
   * Constructs a `NonEmptyList` with the specified head and tail.
   */
  def cons[A](head: A, tail: NonEmptyList[A]): NonEmptyList[A] =
    Cons(head, tail)

  /**
   * Constructs a `NonEmptyList` from the `::` case of a `List`.
   */
  def fromCons[A](cons: ::[A]): NonEmptyList[A] =
    unfold(cons)(_.head) {
      case _ :: h :: t => Some(::(h, t))
      case _           => None
    }

  /**
   * Constructs a `NonEmptyList` from an `Iterable` or `None` otherwise.
   */
  def fromIterableOption[A](iterable: Iterable[A]): Option[NonEmptyList[A]] =
    iterable.toList match {
      case Nil    => None
      case h :: t => Some(fromCons(::(h, t)))
    }

  /**
   * Constructs a `NonEmptyList` from an element and `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptyList[A] =
    fromCons(::(head, tail.toList))

  /**
   * Constructs a `NonEmptyList` from a `NonEmptyChunk`.
   */
  def fromNonEmptyChunk[A](nonEmptyChunk: NonEmptyChunk[A]): NonEmptyList[A] =
    nonEmptyChunk.reduceMapRight(single)(cons)

  /**
   * Constructs a `NonEmptyList` from an initial state `start` by repeatedly
   * applying `iterate` as long as it returns `Some`.
   */
  def iterate[A](start: A)(iterate: A => Option[A]): NonEmptyList[A] =
    unfold(start)(identity)(iterate)

  /**
   * Constructs a `NonEmptyList` with the specified single value.
   */
  def single[A](head: A): NonEmptyList[A] =
    Single(head)

  /**
   * Provides an implicit conversion from `NonEmptyList` to the `::` case of
   * `List` for interoperability with Scala's collection library.
   */
  implicit def toCons[A](nonEmptyList: NonEmptyList[A]): ::[A] =
    nonEmptyList.toCons

  /**
   * Constructs a `NonEmptyList` from an initial state `start` by repeatedly
   * applying `iterate` as long as it returns `Some`, using the function
   * `project` to map each `S` value to an `A` value.
   */
  def unfold[S, A](start: S)(project: S => A)(iterate: S => Option[S]): NonEmptyList[A] = {

    @tailrec
    def loop(start: S, tail: NonEmptyList[A]): NonEmptyList[A] =
      iterate(start) match {
        case None    => cons(project(start), tail).reverse
        case Some(s) => loop(s, cons(project(start), tail))
      }

    iterate(start) match {
      case None    => single(project(start))
      case Some(s) => loop(s, single(project(start)))
    }
  }

  private val nonEmptyListSeed: Int =
    27515742
}

trait LowPriorityNonEmptyListImplicits {

  /**
   * The `CommutativeBoth` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListCommutativeBoth: CommutativeBoth[NonEmptyList] =
    new CommutativeBoth[NonEmptyList] {
      def both[A, B](fa: => NonEmptyList[A], fb: => NonEmptyList[B]): NonEmptyList[(A, B)] =
        fa.zip(fb)
    }

  /**
   * Derives an `Ord[NonEmptyList[A]]` given an `Ord[A]`.
   */
  implicit def NonEmptyListOrd[A: Ord]: Ord[NonEmptyList[A]] =
    Ord[List[A]].contramap(_.toList)

  /**
   * Derives a `PartialOrd[NonEmptyList[A]]` given a `PartialOrd[A]`.
   */
  implicit def NonEmptyListPartialOrd[A: PartialOrd]: PartialOrd[NonEmptyList[A]] =
    PartialOrd[List[A]].contramap(_.toList)
}

trait NonEmptyListSyntax {
  implicit final class NonEmptyListListOps[A](self: List[A]) {

    /**
     * Converts to a `NonEmptyList` or `None` if empty.
     */
    def toNonEmptyList: Option[NonEmptyList[A]] = NonEmptyList.fromIterableOption(self)
  }
  implicit final class NonEmptyListConsOps[A](self: ::[A]) {

    /**
     * Converts to a `NonEmptyList`.
     */
    def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromCons(self)
  }
}
