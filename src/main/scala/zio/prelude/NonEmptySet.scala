package zio.prelude

import scala.language.implicitConversions

import zio.NonEmptyChunk

final class NonEmptySet[A] private (private val set: Set[A]) { self =>

  /** Converts this `NonEmptySet` to a `Set`. */
  def toSet: Set[A] = set

  /**
   * Returns an element of this `NonEmptySet` and the remainder, which is a (possibly empty) `Set`.
   */
  @inline
  def destruct: (A, Set[A]) = (set.head, set.tail)

  /**
   * Converts this `NonEmptySet` to a `NonEmptyChunk`.
   */
  def toNonEmptyChunk: NonEmptyChunk[A] = destruct match { case (head, tail) => NonEmptyChunk.fromIterable(head, tail) }

  /**
   * Converts this `NonEmptySet` to a `NonEmptyList`.
   */
  def toNonEmptyList: NonEmptyList[A] = destruct match { case (head, tail) => NonEmptyList.fromIterable(head, tail) }

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
   * Computes the union between of `NonEmptySet` and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new `NonEmptySet` consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: Set[A]): NonEmptySet[A] = new NonEmptySet(set.union(that))

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

  /** Removes the `elem` from this `NonEmptySet`. Alias for `-`. */
  def remove(elem: A): Set[A] = set - elem

  /**
   * Flattens a `NonEmptySet` of `NonEmptySet` values into a single
   * `NonEmptySet`.
   */
  def flatten[B](implicit ev: A <:< NonEmptySet[B]): NonEmptySet[B] =
    new NonEmptySet[B](set.foldLeft[Set[B]](Set.empty)((b, a) => b union ev(a)))

  override def hashCode: Int = set.hashCode ^ NonEmptySet.NonEmptySetSeed

  @SuppressWarnings(Array("scalafix:DisableSyntax.=="))
  override def equals(that: Any): Boolean =
    that match {
      case that: AnyRef if self.eq(that) => true
      case that: NonEmptySet[A]          => self.set == that.toSet
      case _                             => false
    }

  override def toString: String = s"NonEmpty$set"
}

object NonEmptySet {
  private def apply[A](elem: A, others: Set[A]): NonEmptySet[A] = new NonEmptySet(others + elem)

  /**
   * Creates a `NonEmptySet` with the specified elements.
   *  @tparam A      the type of the `NonEmptySet`'s elements
   *  @param elem    an element of the created `NonEmptySet`
   *  @param others  the remaining elements of the created `NonEmptySet`
   *  @return a new `NonEmptySet` with elements `elem` and `others`
   */
  def apply[A](elem: A, others: A*): NonEmptySet[A] = apply(elem, others.toSet)

  def unapply[A](arg: NonEmptySet[A]): Some[(A, Set[A])] = Some(arg.destruct)

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
    val (head, tail) = l.destruct
    NonEmptySet.fromSet(head, tail.union(r))
  }

  /** Creates a `NonEmptySet` containing elements from `l` and `r` */
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

  private val NonEmptySetSeed: Int = 1247820194
}

trait NonEmptySetSyntax {
  implicit class IterableOps[A](private val iterable: Iterable[A]) {

    /**
     * Constructs a `NonEmptySet` from an `Iterable` or `None` otherwise.
     */
    def toNonEmptySet: Option[NonEmptySet[A]] = NonEmptySet.fromIterableOption(iterable)
  }
}
