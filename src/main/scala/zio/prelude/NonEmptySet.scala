package zio.prelude

import scala.language.implicitConversions

sealed abstract case class NonEmptySet[A] private (elem: A, others: Set[A]) extends (A => Boolean) {
  def toSet: Set[A]                   = others + elem
  def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromIterable(elem, others)
  def contains(elem0: A): Boolean     = toSet.contains(elem0)
  def add(elem0: A): NonEmptySet[A]   = NonEmptySet.fromSet(elem0, toSet)
  def remove(elem0: A): Set[A]        = toSet - elem0
  def size: Int                       = others.size + 1

  /**
   * Flattens a `NonEmptySet` of `NonEmptySet` values into a single
   * `NonEmptySet`.
   */
  final def flatten[B](implicit ev: A <:< NonEmptySet[B]): NonEmptySet[B] =
    others.foldLeft(ev(elem)) { (b, a) =>
      NonEmptySet.union(b, ev(a))
    }

  override def apply(elem0: A): Boolean = contains(elem0)

  override def hashCode(): Int = toSet.hashCode()

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this.eq(that) => true
    case that: NonEmptySet[A]          => this.toSet == that.toSet
    case _                             => false
  }

  override def toString: String = s"NonEmpty$toSet"
}

object NonEmptySet {
  private def apply[A](elem: A, others: Set[A]): NonEmptySet[A] = new NonEmptySet(elem, others - elem) {}
  def apply[A](elem: A, others: A*): NonEmptySet[A]             = apply(elem, others.toSet)

  def fromSet[A](elem: A, others: Set[A]): NonEmptySet[A]   = apply(elem, others - elem)
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

  def union[A](l: NonEmptySet[A], r: Set[A]): NonEmptySet[A] =
    NonEmptySet.fromSet(l.elem, l.others.union(r))
  def union[A](l: NonEmptySet[A], r: NonEmptySet[A]): NonEmptySet[A] =
    union(l, r.toSet)
  def union[A](l: Set[A], r: NonEmptySet[A]): NonEmptySet[A] =
    union(r, l)

  def diff[A](l: NonEmptySet[A], r: Set[A]): Set[A] =
    l.toSet.diff(r)
  def diff[A](l: NonEmptySet[A], r: NonEmptySet[A]): Set[A] =
    diff(l, r.toSet)
  def diff[A](l: Set[A], r: NonEmptySet[A]): Set[A] =
    diff(r, l)

  def intersect[A](l: NonEmptySet[A], r: Set[A]): Set[A] =
    l.toSet.intersect(r)
  def intersect[A](l: NonEmptySet[A], r: NonEmptySet[A]): Set[A] =
    intersect(l, r.toSet)
  def intersect[A](l: Set[A], r: NonEmptySet[A]): Set[A] =
    intersect(r, l)

  def xor[A](l: NonEmptySet[A], r: Set[A]): Set[A] =
    l.toSet.diff(r) union r.diff(l.toSet)
  def xor[A](l: NonEmptySet[A], r: NonEmptySet[A]): Set[A] =
    xor(l, r.toSet)
  def xor[A](l: Set[A], r: NonEmptySet[A]): Set[A] =
    xor(r, l)

  /**
   * The `Associative` instance for `NonEmptySet`.
   */
  implicit def NonEmptySetCommutative[A]: Commutative[NonEmptySet[A]] =
    Commutative.make(_ + _)

  /**
   * Derives a `Debug[NonEmptySet[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptySetDebug[A: Debug]: Debug[NonEmptySet[A]] =
    chunk => Debug.Repr.VConstructor(List("zio", "prelude"), "NonEmptySet", chunk.toNonEmptyList.map(_.debug).toCons)

  /**
   * Derives an `Equal[NonEmptyList[A]]` given an `Equal[A]`.
   */
  implicit def NonEmptySetHash[A]: Hash[NonEmptySet[A]] =
    Hash.default

  /**
   * Provides an implicit conversion from `NonEmptySet` to the `Set`
   * for interoperability with Scala's collection library.
   */
  implicit def toSet[A](nonEmptySet: NonEmptySet[A]): Set[A] =
    nonEmptySet.toSet

}

trait NonEmptySetSyntax {
  implicit class NonEmptySetOps[A](private val l: NonEmptySet[A]) {
    def +(r: NonEmptySet[A]): NonEmptySet[A] = NonEmptySet.union(l, r)
    def +(r: Set[A]): NonEmptySet[A]         = NonEmptySet.union(l, r)

    def -(r: NonEmptySet[A]): Set[A] = NonEmptySet.diff(l, r)
    def -(r: Set[A]): Set[A]         = NonEmptySet.diff(l, r)

    def &(r: NonEmptySet[A]): Set[A] = NonEmptySet.intersect(l, r)
    def &(r: Set[A]): Set[A]         = NonEmptySet.intersect(l, r)

    def ^(r: NonEmptySet[A]): Set[A] = NonEmptySet.xor(l, r)
    def ^(r: Set[A]): Set[A]         = NonEmptySet.xor(l, r)
  }
}
