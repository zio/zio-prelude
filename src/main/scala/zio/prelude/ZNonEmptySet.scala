package zio.prelude

import zio.prelude.newtypes.{ Max, Min, Prod, Sum }

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.HashMap
import scala.language.implicitConversions

/**
 * Similar to `ZSet`, a `ZNonEmptySet[A, B]` is a guaranteed non-empty set of `A` values where `B` represents some notion of
 * "how many" `A` values are included in the set. This can be the number of
 * times each element appears in the set if `B` is a natural number, the
 * probability associated with an element in the set if `B` is a rational
 * number, or even whether an element appears at all if `B` is a boolean.
 */
final class ZNonEmptySet[+A, +B] private (private val map: HashMap[A @uncheckedVariance, B]) { self =>

  /**
   * A symbolic alias for `zip`.
   */
  def <*>[B1 >: B, C](
    that: ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[(A, C), B1] =
    self zip that

  /**
   * A symbolic alias for `combine`.
   */
  def <>[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[A1, B1] =
    self combine that

  /**
   * A symbolic alias for `union`.
   */
  def |[A1 >: A, B1 >: B](
    that: ZSet[A1, B1]
  )(implicit ev1: Commutative[Max[B1]], ev2: Identity[Sum[B1]]): ZNonEmptySet[A1, B1] =
    self union that

  /**
   * Returns the number of times the specified element appears in the set.
   */
  def apply[A1 >: A, B1 >: B](a: A1)(implicit ev: Identity[Sum[B1]]): B1 =
    map.asInstanceOf[Map[A1, B1]].getOrElse(a, ev.identity)

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the sum of the number of times it
   * appears in this set and the specified set.
   */
  def combine[A1 >: A, B1 >: B](that: ZSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[A1, B1] =
    new ZNonEmptySet(that.toMap.foldLeft(self.map.asInstanceOf[HashMap[A1, B1]]) {
      case (map, (a, b1)) =>
        map.get(a) match {
          case Some(b) => map + (a -> ev.combine(Sum(b), Sum(b1)))
          case None    => map + (a -> b1)
        }
    })

  /**
   * Returns whether this set is equal to the specified set, meaning that the
   * same elements appear in both sets the same number of times.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: ZNonEmptySet[_, _] => self.map == that.map
      case _                        => false
    }

  /**
   * Creates a new set for each element in this set and combines the resulting
   * sets together. The number of times each element appears will be the sum
   * of the products of the number of times it appeared in the original set
   * and the number of times it appears in each new set.
   */
  def flatMap[B1 >: B, C](
    f: A => ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[C, B1] =
    map.tail.foldLeft[ZNonEmptySet[C, B1]](map.head match {
      case (a, b) => f(a).transform(b1 => ev2.combine(Prod(b), Prod(b1)))
    }) {
      case (set, (a, b)) =>
        set <> f(a).transform(b1 => ev2.combine(Prod(b), Prod(b1)))
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
  def intersect[A1 >: A, B1 >: B](
    that: ZNonEmptySet[A1, B1]
  )(implicit ev1: Commutative[Min[B1]], ev2: Identity[Sum[B1]]): ZNonEmptySet[A1, B1] =
    new ZNonEmptySet((self.map.toVector ++ that.map.toVector).foldLeft(HashMap.empty[A1, B1]) {
      case (map, (a, b)) =>
        map + (a -> ev1.combine(Min(map.getOrElse(a, ev2.identity)), Min(b)))
    })

  /**
   * Transforms the elements in the set using the specified function. If this
   * results in mapping two or more elements to the same values, the number of
   * times the new value appears in the set will be the sum of the number of
   * times each of the old values appeared in the set.
   */
  def map[B1 >: B, C](f: A => C)(implicit ev: Commutative[Sum[B1]]): ZNonEmptySet[C, B1] =
    new ZNonEmptySet(map.foldLeft[HashMap[C, B1]](HashMap.empty) {
      case (map, (a, b1)) =>
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
  def transform[C](f: B => C): ZNonEmptySet[A, C] =
    new ZNonEmptySet(map.map { case (a, b) => (a, f(b)) })

  /**
   * Converts this set to a `Map` from elements to how many times they appear
   * in the set.
   */
  def toMap[A1 >: A]: Map[A1, B] =
    map.asInstanceOf[Map[A1, B]]

  /**
   * Converts this set to a `Set`, discarding information about how many times
   * an element appears in the set beyond whether it appears at all.
   */
  def toNonEmptySet[A1 >: A, B1 >: B](implicit ev1: Equal[B1], ev2: Identity[Sum[B1]]): NonEmptySet[A1] =
    map.tail.foldLeft(NonEmptySet.single[A1](map.head._1)) {
      case (set, (a, b)) =>
        if (ev1.notEqual(b, ev2.identity)) set + a else set
    }

  /**
   * Returns a meaningful string representation of this set.
   */
  override def toString: String =
    map.mkString("ZNonEmptySet(", ", ", ")")

  def toZSet: ZSet[A, B] = ZSet.fromMap(map)

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the maximum of the number of times
   * it appears in this set and the specified set.
   */
  def union[A1 >: A, B1 >: B](
    that: ZSet[A1, B1]
  )(implicit ev1: Commutative[Max[B1]], ev2: Identity[Sum[B1]]): ZNonEmptySet[A1, B1] =
    new ZNonEmptySet((self.map.toVector ++ that.toMap.toVector).foldLeft(HashMap.empty[A1, B1]) {
      case (map, (a, b)) =>
        map + (a -> ev1.combine(Max(map.getOrElse(a, ev2.identity)), Max(b)))
    })

  /**
   * Combines this set with the specified set to produce their cartesian
   * product.
   */
  def zip[B1 >: B, C](
    that: ZNonEmptySet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[(A, C), B1] =
    zipWith(that)((_, _))

  /**
   * Combines this set with the specified set to produce their cartesian
   * product, combining pair of elements using the specified function `f`.
   */
  def zipWith[B1 >: B, C, D](
    that: ZNonEmptySet[C, B1]
  )(f: (A, C) => D)(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): ZNonEmptySet[D, B1] =
    self.flatMap(a => that.map(c => f(a, c)))
}

object ZNonEmptySet {

  /**
   * Constructs a set with the specified elements.
   */
  def apply[A](a: A, as: A*): NonEmptyMultiSet[A] =
    fromIterable(a, as)

  /**
   * The singleton set.
   */
  def single[A](a: A): NonEmptyMultiSet[A] =
    new ZNonEmptySet(HashMap((a, 1)))

  /**
   * Constructs a set from the specified `Iterable`. The measure of how many
   * times a value occurs in the set will be an integer representing how many
   * times the value occurred in the specified `Iterable`.
   */
  def fromIterable[A](head: A, tail: Iterable[A]): NonEmptyMultiSet[A] =
    new ZNonEmptySet(tail.foldLeft(HashMap((head, 1)))((map, a) => map + (a -> map.get(a).fold(1)(_ + 1))))

  /**
   * Constructs a set from the specified `Iterable`. The measure of how many
   * times a value occurs in the set will be an integer representing how many
   * times the value occurred in the specified `Iterable`. Returns `None` if empty.
   */
  def fromIterableOption[A](elems: Iterable[A]): Option[NonEmptyMultiSet[A]] =
    if (elems.isEmpty)
      None
    else
      Some(
        new ZNonEmptySet(
          elems.tail.foldLeft(HashMap((elems.head, 1)))((map, a) => map + (a -> map.get(a).fold(1)(_ + 1)))
        )
      )

  /**
   * Constructs a set from the specified `Set`. The measure of how many times
   * a value occurs in the set will be a boolean representing whether a value
   * occurs at all.
   */
  def fromNonEmptySet[A](set: NonEmptySet[A]): ZNonEmptySet[A, Boolean] =
    new ZNonEmptySet(set.toSet.foldLeft(HashMap.empty[A, Boolean])((map, a) => map + (a -> true)))

  /**
   * Constructs a set from the specified `Map`. The values will be the keys in
   * the `Map` and the measure of how many times a value occurs will be the
   * keys value. Returns `None` if empty.
   */
  def fromMapOption[A, B](map: Map[A, B]): Option[ZNonEmptySet[A, B]] =
    if (map.isEmpty)
      None
    else
      Some(
        new ZNonEmptySet(
          map match {
            case map: HashMap[A, B] => map
            case _                  => map.foldLeft(HashMap.empty[A, B])(_ + _)
          }
        )
      )

  /** Constructs a `NonEmptyMultiSet`, where, by definition, each element is present exactly once.
   * Returns `None` if empty.
   */
  def fromSetOption[A](set: Set[A]): Option[NonEmptyMultiSet[A]] =
    if (set.isEmpty)
      None
    else
      Some(
        new ZNonEmptySet(HashMap[A, Int](set.toSeq.map((_, 1)): _*))
      )

  /**
   * Derives a `Commutative[ZNonEmptySet[A, B]]` given a `Commutative[B]`.
   */
  implicit def ZNonEmptySetCommutative[A, B: Commutative]: Commutative[ZNonEmptySet[A, B]] =
    new Commutative[ZNonEmptySet[A, B]] {
      def combine(left: => ZNonEmptySet[A, B], right: => ZNonEmptySet[A, B]): ZNonEmptySet[A, B] =
        new ZNonEmptySet(right.map.foldLeft(left.map) {
          case (map, (a, b1)) =>
            map.get(a) match {
              case Some(b) => map + (a -> (b <> b1))
              case None    => map + (a -> b1)
            }
        })
    }

  /**
   * Derives an `Equal[ZNonEmptySet[A, B]]` given an `Equal[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZNonEmptySetEqual[A, B: Equal]: Equal[ZNonEmptySet[A, B]] =
    Equal[HashMap[A, B]].contramap(_.map)

  /**
   * The `EqualF` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetDeriveEqual[B: Equal]: DeriveEqual[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new DeriveEqual[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def derive[A: Equal]: Equal[ZNonEmptySet[A, B]] =
        ZNonEmptySetEqual
    }

  /**
   * The `Covariant` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetCovariant[B](
    implicit ev: Commutative[Sum[B]]
  ): Covariant[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new Covariant[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def map[A, C](f: A => C): ZNonEmptySet[A, B] => ZNonEmptySet[C, B] =
        _.map(f)
    }

  /**
   * The `IdentityFlatten` instance for `ZNonEmptySet`.
   */
  implicit def ZNonEmptySetIdentityFlatten[B](
    implicit ev1: Commutative[Sum[B]],
    ev2: Commutative[Prod[B]]
  ): IdentityFlatten[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] =
    new IdentityFlatten[({ type lambda[+x] = ZNonEmptySet[x, B] })#lambda] {
      def flatten[A](ffa: ZNonEmptySet[ZNonEmptySet[A, B], B]): ZNonEmptySet[A, B] =
        ffa.flatMap(identity)
      def any: ZNonEmptySet[Any, B] = ???

    }

  /**
   * Derives a `Hash[ZNonEmptySet[A, B]]` given a `Hash[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def ZNonEmptySetHash[A, B: Hash]: Hash[ZNonEmptySet[A, B]] =
    Hash[HashMap[A, B]].contramap(_.map)

  /**
   * Provides an implicit conversion from `NonEmptySet` to the `Set`
   * for interoperability with Scala's collection library.
   */
  implicit def toZSet[A, B](zNonEmptySet: ZNonEmptySet[A, B]): ZSet[A, B] =
    zNonEmptySet.toZSet
}
