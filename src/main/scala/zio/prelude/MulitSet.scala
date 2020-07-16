package zio.prelude

import scala.annotation.unchecked.uncheckedVariance

import zio.prelude.newtypes.{ Max, Min, Prod, Sum }

/**
 * A `MultiSet[A, B]` is a set of `A` values where `B` represents some notion
 * of "how many" `A` values are included in the set. This can be the number of
 * times each element appears in the set if `B` is a natural number, the
 * probability associated with an element in the set if `B` is a rational
 * number, or even whether an element appears at all if `B` is a boolean.
 */
final class MultiSet[+A, +B] private (private val map: Map[A @uncheckedVariance, B]) { self =>

  /**
   * A symbolic alias for `intersect`.
   */
  def &[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Min[B1]]): MultiSet[A1, B1] =
    self intersect that

  /**
   * A symbolic alias for `diff`.
   */
  def &~[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Inverse[B1]): MultiSet[A1, B1] =
    self diff that

  /**
   * A symbolic alias for `zip`.
   */
  def <*>[B1 >: B, C](
    that: MultiSet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): MultiSet[(A, C), B1] =
    self zip that

  /**
   * A symbolic alias for `combine`.
   */
  def <>[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): MultiSet[A1, B1] =
    self combine that

  /**
   * A symbolic alias for `union`.
   */
  def |[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Max[B1]]): MultiSet[A1, B1] =
    self union that

  /**
   * Returns the number of times the specified element appears in the set.
   */
  def apply[A1 >: A, B1 >: B](a: A1)(implicit ev: Identity[Sum[B1]]): B1 =
    map.asInstanceOf[Map[A1, B1]].get(a).getOrElse(Sum.unwrap(ev.identity))

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the sum of the number of times it
   * appears in this set and the specified set.
   */
  def combine[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Sum[B1]]): MultiSet[A1, B1] =
    MultiSet {
      that.map.foldLeft(self.map.asInstanceOf[Map[A1, B1]]) {
        case (map, (a, b1)) =>
          map.get(a) match {
            case Some(b) => map + (a -> ev.combine(Sum.wrap(b), Sum.wrap(b1)))
            case None    => map + (a -> b1)
          }
      }
    }

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the difference between the number
   * of times it appears in this set and the specified set.
   */
  def diff[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Inverse[B1]): MultiSet[A1, B1] =
    MultiSet {
      that.map.foldLeft(self.map.asInstanceOf[Map[A1, B1]]) {
        case (map, (a, b1)) =>
          map.get(a) match {
            case Some(b) => map + (a -> ev.combine(b, ev.inverse(b1)))
            case None    => map + (a -> ev.inverse(b1))
          }
      }
    }

  /**
   * Returns whether this set is equal to the specified set, meaning that the
   * same elements appear in both sets the same number of times.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: MultiSet[_, _] => self.map == that.map
      case _                    => false
    }

  /**
   * Creates a new set for each element in this set and combines the resulting
   * sets together. The number of times each element appears will be the sum
   * of the products of the number of times it appeared in the original set
   * and the number of times it appears in each new set.
   */
  def flatMap[B1 >: B, C](
    f: A => MultiSet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): MultiSet[C, B1] =
    map.foldLeft[MultiSet[C, B1]](MultiSet.empty) {
      case (set, (a, b)) =>
        set <> f(a).transform(b1 => ev2.combine(Prod.wrap(b), Prod.wrap(b1)))
    }

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the minimum of the number of times
   * it appears in this set and the specified set.
   */
  def intersect[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Min[B1]]): MultiSet[A1, B1] =
    MultiSet {
      self.map.foldLeft(that.map) {
        case (map, (a, b)) =>
          map.get(a).fold(map)(b1 => map + (a -> ev.combine(Min.wrap(b), Min.wrap(b1))))
      }
    }

  /**
   * Transforms the elements in the set using the specified function. If this
   * results in mapping two or more elements to the same values, the number of
   * times the new value appears in the set will be the sum of the number of
   * times each of the old values appeared in the set.
   */
  def map[B1 >: B, C](f: A => C)(implicit ev: Commutative[Sum[B1]]): MultiSet[C, B1] =
    MultiSet {
      map.foldLeft[Map[C, B1]](Map.empty) {
        case (map, (a, b1)) =>
          val c = f(a)
          map.get(c) match {
            case Some(b) => map + (c -> ev.combine(Sum.wrap(b), Sum.wrap(b1)))
            case None    => map + (c -> b1)
          }
      }
    }

  /**
   * Transforms the representation of how many times each element appears in
   * the set with the specified function.
   */
  def transform[C](f: B => C): MultiSet[A, C] =
    MultiSet {
      map.map { case (a, b) => (a, f(b)) }
    }

  /**
   * Returns a meaningful string representation of this set.
   */
  override def toString: String =
    map.mkString("MultiSet(", ", ", ")")

  /**
   * Combines this set with the specified set to produce a new set where the
   * number of times each element appears is the maximum of the number of times
   * it appears in this set and the specified set.
   */
  def union[A1 >: A, B1 >: B](that: MultiSet[A1, B1])(implicit ev: Commutative[Max[B1]]): MultiSet[A1, B1] =
    MultiSet {
      self.map.foldLeft(that.map) {
        case (map, (a, b)) =>
          map.get(a).fold(map + (a -> b))(b1 => map + (a -> ev.combine(Max.wrap(b), Max.wrap(b1))))
      }
    }

  /**
   * Combines this set with the specified set to produce their cartesian
   * product.
   */
  def zip[B1 >: B, C](
    that: MultiSet[C, B1]
  )(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): MultiSet[(A, C), B1] =
    zipWith(that)((_, _))

  /**
   * Combines this set with the specified set to produce their cartesian
   * product, combining pair of elements using the specified function `f`.
   */
  def zipWith[B1 >: B, C, D](
    that: MultiSet[C, B1]
  )(f: (A, C) => D)(implicit ev1: Commutative[Sum[B1]], ev2: Commutative[Prod[B1]]): MultiSet[D, B1] =
    self.flatMap(a => that.map(c => f(a, c)))
}

object MultiSet {

  /**
   * Constructs a set from the specified `Map`.
   */
  def apply[A, B](map: Map[A, B]): MultiSet[A, B] =
    new MultiSet(map)

  /**
   * Constructs a set with the specified elements.
   */
  def apply[A](as: A*): MultiSet[A, Int] =
    fromIterable(as)

  /**
   * Constructs an empty set with the specified element type.
   */
  def empty[A]: MultiSet[A, Nothing] =
    MultiSet(Map.empty)

  /**
   * Constructs a set from the specified `Iterable`.
   */
  def fromIterable[A](as: Iterable[A]) =
    MultiSet(as.foldLeft(Map.empty[A, Int])((map, a) => map + (a -> map.get(a).fold(1)(_ + 1))))

  /**
   * Derives a `Commutative[MultiSet[A, B]]` given a `Commutative[B]`.
   */
  implicit def MultiSetCommutative[A, B: Commutative]: Commutative[MultiSet[A, B]] =
    new Commutative[MultiSet[A, B]] {
      def combine(left: => MultiSet[A, B], right: => MultiSet[A, B]): MultiSet[A, B] =
        MultiSet {
          right.map.foldLeft(left.map) {
            case (map, (a, b1)) =>
              map.get(a) match {
                case Some(b) => map + (a -> (b <> b1))
                case None    => map + (a -> b1)
              }
          }
        }
    }

  /**
   * Derives an `Equal[MultiSet[A, B]]` given an `Equal[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def MultiSetEqual[A, B: Equal]: Equal[MultiSet[A, B]] =
    Equal[Map[A, B]].contramap(_.map)

  /**
   * The `EqualF` instance for `MultiSet`.
   */
  implicit def MultiSetEqualF[B: Equal]: EqualF[({ type lambda[+x] = MultiSet[x, B] })#lambda] =
    new EqualF[({ type lambda[+x] = MultiSet[x, B] })#lambda] {
      def deriveEqual[A: Equal]: Equal[MultiSet[A, B]] =
        MultiSetEqual
    }

  /**
   * The `Covariant` instance for `MultiSet`.
   */
  implicit def MultiSetCovariant[B](
    implicit ev: Commutative[Sum[B]]
  ): Covariant[({ type lambda[+x] = MultiSet[x, B] })#lambda] =
    new Covariant[({ type lambda[+x] = MultiSet[x, B] })#lambda] {
      def map[A, C](f: A => C): MultiSet[A, B] => MultiSet[C, B] =
        _.map(f)
    }

  /**
   * Derives a `Hash[MultiSet[A, B]]` given a `Hash[B]`. Due to the
   * limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def MultiSetHash[A, B: Hash]: Hash[MultiSet[A, B]] =
    Hash[Map[A, B]].contramap(_.map)
}
