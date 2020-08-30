package zio.prelude

import scala.language.implicitConversions

final class NonEmptySet[A] private (val toSet: Set[A]) {
  def destruct: (A, Set[A])                            = (toSet.head, toSet.tail)
  def toNonEmptyList: NonEmptyList[A]                  = destruct match { case (head, tail) => NonEmptyList.fromIterable(head, tail) }
  def add(elem: A): NonEmptySet[A]                     = new NonEmptySet(toSet + elem)
  def +(elem: A): NonEmptySet[A]                       = new NonEmptySet(toSet + elem)
  def +(elem1: A, elem2: A, elems: A*): NonEmptySet[A] = new NonEmptySet(toSet + elem1 + elem2 ++ elems)
  def union(that: Set[A]): NonEmptySet[A]              = new NonEmptySet(toSet.union(that))
  def ++(that: Set[A]): NonEmptySet[A]                 = new NonEmptySet(toSet ++ that)
  def remove(elem: A): Set[A]                          = toSet - elem

  /**
   * Flattens a `NonEmptySet` of `NonEmptySet` values into a single
   * `NonEmptySet`.
   */
  def flatten[B](implicit ev: A <:< NonEmptySet[B]): NonEmptySet[B] =
    destruct match {
      case (head, tail) =>
        tail.foldLeft(ev(head)) { (b, a) =>
          b ++ ev(a)
        }
    }

  override def hashCode: Int = toSet.hashCode()

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this.eq(that) => true
    case that: NonEmptySet[A]          => this.toSet == that.toSet
    case _                             => false
  }

  override def toString: String = s"NonEmpty$toSet"
}

object NonEmptySet {
  private def apply[A](elem: A, others: Set[A]): NonEmptySet[A] = new NonEmptySet(others + elem)
  def apply[A](elem: A, others: A*): NonEmptySet[A]             = apply(elem, others.toSet)

  def unapply[A](arg: NonEmptySet[A]): Some[(A, Set[A])] = Some(arg.destruct)

  def fromSet[A](elem: A, others: Set[A]): NonEmptySet[A]   = apply(elem, others)
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

  def union[A](l: NonEmptySet[A], r: Set[A]): NonEmptySet[A] = {
    val (head, tail) = l.destruct
    NonEmptySet.fromSet(head, tail.union(r))
  }

  def union[A](l: Set[A], r: NonEmptySet[A]): NonEmptySet[A] =
    union(r, l)

  /**
   * The `Associative` instance for `NonEmptySet`.
   */
  implicit def NonEmptySetCommutative[A]: Commutative[NonEmptySet[A]] =
    Commutative.make(_ ++ _)

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
  implicit class SetOps[A](private val l: Set[A]) {
    def ++(r: NonEmptySet[A]): NonEmptySet[A]    = NonEmptySet.union(l, r)
    def union(r: NonEmptySet[A]): NonEmptySet[A] = NonEmptySet.union(l, r)
  }
}
