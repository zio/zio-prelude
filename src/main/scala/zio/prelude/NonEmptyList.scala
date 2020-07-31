package zio.prelude

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.hashing.MurmurHash3

import zio.NonEmptyChunk
import zio.prelude.NonEmptyList._
import zio.prelude.newtypes.{ Max, Min, Prod, Sum }

/**
 * A `NonEmptyList[A]` is a list of one or more values of type A. Unlike a
 * `List`, a `NonEmptyList` is guaranteed to contain at least one element.
 * This additional structure allows some operations to be defined on
 * `NonEmptyList` that are not safe on `List`, such as `head` and `reduce`.
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
      case (Cons(h1, t1), Cons(h2, t2)) if (f(h1, h2)) => t1.corresponds(t2)(f)
      case (Single(h1), Single(h2))                    => f(h1, h2)
      case _                                           => false
    }

  /**
   * Returns the number of elements in this `NonEmptyList` that satisfy the
   * specified predicate.
   */
  final def count(f: A => Boolean): Int =
    foldLeft(0)((n, a) => if (f(a)) n + 1 else n)

  /**
   * Returns whether this `NonEmptyList` and the specified `NonEmptyList` are
   * equal to each other.
   */
  override final def equals(that: Any): Boolean =
    that match {
      case that: NonEmptyList[_] => self.corresponds(that)(_ == _)
      case _                     => false
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
  final def foreach[F[+_]: AssociativeBoth: Covariant, B](f: A => F[B]): F[NonEmptyList[B]] =
    reduceMapRight(f(_).map(single))((a, fas) => f(a).zipWith(fas)(cons))

  /**
   * Returns the hashCode of this `NonEmptyList`.
   */
  override final def hashCode: Int = {
    val (hash, n) = foldLeft((nonEmptyListSeed, 0)) {
      case ((hash, n), a) =>
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
    implicit val A = B.contramap(f)
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
    implicit val A = B.contramap(f)
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
   * function `reduce` to combine the `B` value with each other `A` value.
   */
  final def reduceMapLeft[B](map: A => B)(reduce: (B, A) => B): B =
    self match {
      case Cons(h, t) => t.foldLeft(map(h))(reduce)
      case Single(t)  => map(t)
    }

  /**
   * Reduces the elements of this `NonEmptyList` from right to left using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduce` to combine the `B` value with each other `A` value.
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
  final def tailOption: Option[NonEmptyList[A]] =
    self match {
      case Cons(_, t) => Some(t)
      case _          => None
    }

  /**
   * Returns a new `NonEmptyList` composed of this `NonEmptyList` followed by
   * each of its tails, ending with a singleton `NonEmptyList`.
   */
  final def tails: NonEmptyList[NonEmptyList[A]] =
    unfold(self)(identity)(_.tailOption)

  /**
   * Converts this `NonEmptyList` to the `::` case of a `List`.
   */
  final def toCons[A1 >: A]: ::[A1] =
    reduceMapRight[::[A1]](::(_, Nil))(::(_, _))

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
    unfold((self, that)) {
      case (l, r) => f(l.head, r.head)
    } {
      case (Cons(_, t1), Cons(_, t2)) => Some((t1, t2))
      case _                          => None
    }

  /**
   * Annotates each element of this `NonEmptyList` with its index.
   */
  final def zipWithIndex: NonEmptyList[(A, Int)] =
    unfold((self, 0)) {
      case (as, n) => (as.head, n)
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
   * The `AssociativeBoth` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListAssociativeBoth: AssociativeBoth[NonEmptyList] =
    new AssociativeBoth[NonEmptyList] {
      def both[A, B](fa: => NonEmptyList[A], fb: => NonEmptyList[B]): NonEmptyList[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityFlatten` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListIdentityFlatten: IdentityFlatten[NonEmptyList] =
    new IdentityFlatten[NonEmptyList] {
      val any: NonEmptyList[Any] = single(())
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
   * The `IdentityBoth` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListIdentityBoth: IdentityBoth[NonEmptyList] =
    new IdentityBoth[NonEmptyList] {
      val any: NonEmptyList[Any] =
        single(())
      def both[A, B](fa: => NonEmptyList[A], fb: => NonEmptyList[B]): NonEmptyList[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `NonEmptyTraversable` instance for `NonEmptyList`.
   */
  implicit val NonEmptyListNonEmptyTraversable: NonEmptyTraversable[NonEmptyList] =
    new NonEmptyTraversable[NonEmptyList] {
      def foreach1[F[+_]: AssociativeBoth: Covariant, A, B](fa: NonEmptyList[A])(f: A => F[B]): F[NonEmptyList[B]] =
        fa.foreach(f)
    }

  /**
   * Derives an `Ord[NonEmptyList[A]]` given an `Ord[A]`.
   */
  implicit def NonEmptyListOrd[A: Ord]: Ord[NonEmptyList[A]] = {

    @tailrec
    def loop[A: Ord](left: NonEmptyList[A], right: NonEmptyList[A]): Ordering =
      (left, right) match {
        case (Single(h1), Single(h2))  => Ord[A].compare(h1, h2)
        case (Single(h1), Cons(h2, _)) => Ord[A].compare(h1, h2) <> Ordering.LessThan
        case (Cons(h1, _), Single(h2)) => Ord[A].compare(h1, h2) <> Ordering.GreaterThan
        case (Cons(h1, t1), Cons(h2, t2)) =>
          val compare = Ord[A].compare(h1, h2)
          if (compare.isEqual) loop(t1, t2) else compare
      }

    Ord.make((l, r) => loop(l, r))
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
   * Constructs a `NonEmptyList` from an `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptyList[A] =
    fromCons(::(head, tail.toList))

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
   * Derives a `Hash[NonEmptyList[A]]` given a `Hash[A]`.
   */
  implicit def NonEmptyListHash[A: Hash]: Hash[NonEmptyList[A]] =
    Hash.make(_.map(_.hash).hashCode, _.corresponds(_)(_ === _))
}
