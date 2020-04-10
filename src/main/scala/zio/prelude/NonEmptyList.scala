package zio.prelude

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import zio.{ Chunk, NonEmptyChunk }
import zio.prelude.NonEmptyList._

/**
 * A `NonEmptyList[A]` is a list of one or more values of type A. Unlike a
 * `List`, a `NonEmptyList` is guaranteed to contain at least one element.
 * This additional structure allows some operations to be defined on
 * `NonEmptyList` which are not safe on lists, such as `head` and `reduce`.
 */
sealed trait NonEmptyList[+A] extends Seq[A] { self =>

  /**
   * Concatenates this `NonEmptyList` with the specified `NonEmptyList`.
   */
  final def ++[A1 >: A](that: NonEmptyList[A1]): NonEmptyList[A1] =
    foldRight(that)(cons)

  /**
   * Returns the element at the specified index. Note that this method is not
   * safe because the `NonEmptyList` may not contain the specified index and
   * is provided solely for compatibility with Scala's `Seq`.
   */
  override final def apply(n: Int): A =
    dropOption(n).map(_.head).getOrElse(throw new IndexOutOfBoundsException(n.toString))

  /**
   * Determines whether this `NonEmptyList` and the specified `NonEmptyList`
   * have the same length and every pair of corresponding elements of this
   * `NonEmptyList` and the specified `NonEmptyList satisfy the specified
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
   * Returns this `NonEmptyList` with the specified number of elements
   * dropped, returning `None` if all of the elements were dropped.
   */
  @tailrec
  final def dropOption(n: Int): Option[NonEmptyList[A]] =
    if (n <= 0) Some(self)
    else
      self match {
        case Cons(_, t) => t.dropOption(n - 1)
        case _          => None
      }

  /**
   * Drops elements from the left of this `NonEmptyList` as long as the
   * elements satisfy the specified predicate.
   */
  @tailrec
  final def dropWhileOption(f: A => Boolean): Option[NonEmptyList[A]] =
    if (!f(head)) Some(self)
    else
      self match {
        case Cons(_, t) => t.dropWhileOption(f)
        case _          => None
      }

  /**
   * Transforms each element of this `NonEmptyList` to a `NonEmptyList` and
   * combines them into a single `NonEmptyList`.
   */
  final def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    reduceMapRight(f)((a, z) => f(a) ++ z)

  /**
   * Folds over the elements of this `NonEmptyList` from left to right using
   * the specified initial value and combining function
   */
  @tailrec
  override final def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z =
    self match {
      case Cons(h, t) => t.foldLeft(f(z, h))(f)
      case Single(h)  => f(z, h)
    }

  /**
   * Folds over the elements of this `NonEmptyList` from right to left using
   * the specified initial value and combining function.
   */
  override final def foldRight[B](z: B)(op: (A, B) => B): B =
    self.reverse.foldLeft(z)((b, a) => op(a, b))

  /**
   * Returns whether this `NonEmptyList` has more than one element.
   */
  def hasTail: Boolean

  /**
   * Returns an `Iterator` that iterates over the elements of this
   * `NonEmptyList`. Note that `Iterator` is not a referentially transparent
   * interface and this method is provided solely for compatibility with
   * Scala's `Seq`.
   */
  override final def iterator: Iterator[A] =
    new Iterator[A] {
      var state = self
      def hasNext: Boolean =
        state != null
      def next(): A = {
        val a = state.head
        state = if (state.hasTail) state.tail else null
        a
      }
    }

  /**
   * Returns the length of this `NonEmptyList`.
   */
  override final def length: Int =
    foldLeft(0)((n, _) => n + 1)

  /**
   * Transforms the elements of this `NonEmptyList` with the specified
   * function.
   */
  final def map[B](f: A => B): NonEmptyList[B] =
    reduceMapRight(a => single(f(a)))((a, z) => cons(f(a), z))

  /**
   * Renders the elements of this `NonEmptyList` as a `String`.
   */
  override final def mkString: String =
    mkString("")

  /**
   * Renders the elements of this `NonEmptyList` as a `String` using the
   * specified separator.
   */
  override final def mkString(sep: String): String =
    mkString("", sep, "")

  /**
   * Renders the elements of this `NonEmptyList` as a `String` using the
   * specified separator and start and end values.
   */
  override final def mkString(start: String, sep: String, end: String): String =
    start + reduceMapLeft(_.toString)((b, a) => b + sep + a.toString) + end

  /**
   * Returns the product of the elements of this `NonEmptyList`.
   */
  override final def product[A1 >: A](implicit num: Numeric[A1]): A1 =
    reduce(num.times)

  /**
   * Reduces the elements of this `NonEmptyList` from left to right using the
   * specified function.
   */
  override def reduce[A1 >: A](f: (A1, A1) => A1): A1 =
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
   * function `map` to transform the first value to the type `Z` and then the
   * function `reduce` to combine the `Z` value with each other `A` value.
   */
  final def reduceMapLeft[Z](map: A => Z)(reduce: (Z, A) => Z): Z =
    self match {
      case Cons(h, t) => t.foldLeft(map(h))(reduce)
      case Single(t)  => map(t)
    }

  /**
   * Reduces the elements of this `NonEmptyList` from right to left using the
   * function `map` to transform the first value to the type `Z` and then the
   * function `reduce` to combine the `Z` value with each other `A` value.
   */
  final def reduceMapRight[B](map: A => B)(reduce: (A, B) => B): B =
    self.reverse.reduceMapLeft[B](map)((b, a) => reduce(a, b))

  /**
   * Reverses the order of elements in this `NonEmptyList`.
   */
  override final def reverse: NonEmptyList[A] =
    reduceMapLeft[NonEmptyList[A]](Single(_))((b, a) => Cons(a, b))

  /**
   * Returns the sum of the elements of this `NonEmptyList`.
   */
  override final def sum[A1 >: A](implicit num: Numeric[A1]): A1 =
    reduce(num.plus)

  /**
   * Returns the tail of this `NonEmptyList`. Note that this method is not
   * safe because a `NonEmptyList` with a single element does not have a tail
   * and is provided solely for compatibility with Scala's `Seq`.
   */
  override def tail: NonEmptyList[A] =
    unsafeTail

  /**
   * Returns the tail of this `NonEmptyList` if it exists or `None` otherwise.
   */
  final def tailOption: Option[NonEmptyList[A]] =
    if (hasTail) Some(tail) else None

  /**
   * Returns a new `NonEmptyList` composed of this `NonEmptyList` followed by
   * each of its tails, ending with a singleton `NonEmptyList`.
   */
  final def tails0: NonEmptyList[NonEmptyList[A]] =
    unfold(self)(identity)(_.tailOption)

  /**
   * Converts this `NonEmptyList` to the `::` case of a `List`.
   */
  final def toCons[A1 >: A]: ::[A1] =
    reduceMapRight[::[A1]](::(_, Nil))(::(_, _))

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
   * Converts this `NonEmptyList` to a `List`.
   */
  override final def toList: List[A] =
    foldRight[List[A]](List.empty)(_ :: _)

  /**
   * Renders this `NonEmptyList` as a `String`.
   */
  override final def toString: String =
    mkString("NonEmptyList(", ", ", ")")

  /**
   * Converts this `NonEmptyList` to a `NonEmptyChunk`.
   */
  final def toNonEmptyChunk: NonEmptyChunk[A] =
    self match {
      case Cons(h, t) => Chunk(h, t: _*)
      case Single(h)  => Chunk(h)
    }

  protected def unsafeTail: NonEmptyList[A]
}

object NonEmptyList {

  final case class Cons[+A](override val head: A, override val tail: NonEmptyList[A]) extends NonEmptyList[A] {
    def hasTail: Boolean                               = true
    protected override def unsafeTail: NonEmptyList[A] = tail
  }

  final case class Single[+A](override val head: A) extends NonEmptyList[A] {
    def hasTail: Boolean                      = false
    protected def unsafeTail: NonEmptyList[A] = throw new NoSuchElementException("Single.tail")
  }

  /**
   * The `Associative` instance for `NonEmptyList`.
   */
  implicit def NonEmptyListAssociative[A]: Associative[NonEmptyList[A]] =
    Associative.make(_ ++ _)

  /**
   * Derives a `Debug[NonEmptyList[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptyListDebug[A: Debug]: Debug[NonEmptyList[A]] =
    chunk => Debug.Repr.VConstructor(List("zio", "prelude"), "NonEmptyList", chunk.map[Debug.Repr](_.debug).toCons)

  /**
   * Derives an `Equal[NonEmptyList[A]]` given an `Equal[A]`.
   */
  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] =
    Equal.make(_.corresponds(_)(_ === _))

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
   * Constructs a `NonEmptylist` from the `::` case of a `List`.
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
   * Constructs a `NonEmptyList` with the specified single value.
   */
  def single[A](head: A): NonEmptyList[A] =
    Single(head)

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

  private[prelude] val message =
    "This method is provided solely for compatibility with Scala's `Seq`."
}

trait LowPriorityNonEmptyListImplicits {

  /**
   * Derives a `Hash[NonEmptyList[A]]` given a `Hash[A]`.
   */
  implicit def NonEmptyListHash[A: Hash]: Hash[NonEmptyList[A]] =
    Hash.make(_.map[Int](_.hash).hashCode, _.corresponds(_)(_ === _))
}
