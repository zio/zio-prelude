package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.HashCoherent
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` can be hashed.
 */
@implicitNotFound("No implicit Hash defined for ${A}.")
trait Hash[-A] extends Equal[A] { self =>

  /**
   * Returns the hash of the specified value.
   */
  def hash(a: A): Int

  override protected def checkEqual(l: A, r: A): Boolean

  /**
   * Constructs a `Hash[(A, B)]` given a `Hash[A]` and `Hash[B]` by hashing the
   * `A` and `B` values together.
   */
  def both[B](that: Hash[B]): Hash[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs a `Hash[C]` given a `Hash[A]`, a `Hash[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, and hash the `A` and `B` values together.
   */
  def bothWith[B, C](that: Hash[B])(f: C => (A, B)): Hash[C] =
    Hash.make(
      c =>
        f(c) match {
          case (a, b) => (self.hash(a), that.hash(b)).hashCode
        },
      (c1, c2) =>
        (f(c1), f(c2)) match {
          case ((a1, b1), (a2, b2)) => self.equal(a1, a2) && that.equal(b1, b2)
        }
    )

  /**
   * Constructs a `Hash[B]` given a `Hash[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and hash the `A` values.
   */
  override def contramap[B](f: B => A): Hash[B] =
    Hash.make(
      b => hash(f(b)),
      (b1, b2) => equal(f(b1), f(b2))
    )

  /**
   * Constructs a `Hash[Either[A, B]]` given a `Hash[A]` and a `Hash[B]`. The
   * instance will hash either the `A` or `B` values.
   */
  def either[B](that: Hash[B]): Hash[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs a `Hash[C]` given a `Hash[A]`, a `Hash[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]` and then hash either the `A` or `B`
   * values.
   */
  def eitherWith[B, C](that: Hash[B])(f: C => Either[A, B]): Hash[C] =
    Hash.make(
      c =>
        f(c) match {
          case Left(a)  => Left(self.hash(a)).hashCode
          case Right(b) => Right(that.hash(b)).hashCode
        },
      (c1, c2) =>
        (f(c1), f(c2)) match {
          case (Left(a1), Left(a2))   => self.equal(a1, a2)
          case (Right(b1), Right(b2)) => that.equal(b1, b2)
          case _                      => false
        }
    )
}

object Hash extends Lawful[Hash] with HashCoherent {

  /**
   * For all values `a1` and `a2`, if `a1` is equal to `a2` then the hash of
   * `a1` is equal to the hash of `a2`.
   */
  val consistencyLaw: Laws.Law2[Hash] =
    new Laws.Law2[Hash]("consistencyLaw") {
      def apply[A](a1: A, a2: A)(implicit caps: Hash[A]): TestResult =
        (a1 <-> a2) ==> (Hash[A].hash(a1) <-> Hash[A].hash(a2))
    }

  /**
   * The set of all laws that instances of `Hash` must satisfy.
   */
  val laws: Laws[Hash] =
    consistencyLaw + Equal.laws

  /**
   * Summons an implicit `Hash[A]`.
   */
  def apply[A](implicit hash: Hash[A]): Hash[A] =
    hash

  /**
   * Constructs an instance from a function.
   */
  def make[A](hash0: A => Int, equal0: (A, A) => Boolean): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int                 = hash0(a)
      def checkEqual(l: A, r: A): Boolean = equal0(l, r)
    }

  /**
   * Constructs a `Hash[A]` that uses the default notion of hashing embodied in
   * the implementation of `hashCode` for values of type `A`.
   */
  def default[A]: Hash[A] =
    make(_.hashCode(), _ == _)

  /**
   * Hashing for `Boolean` values.
   */
  implicit val BooleanHash: Hash[Boolean] =
    default

  /**
   * Hashing for `Byte` values.
   */
  implicit val ByteHas: Hash[Byte] =
    default

  /**
   * Hashing for `Char` values.
   */
  implicit val CharHash: Hash[Char] =
    default

  /**
   * Hashing for `Double` values.
   */
  implicit val DoubleHash: Hash[Double] =
    default

  /**
   * Derives a `Hash[Either[A, B]]` given a `Hash[A]` and a `Hash[B]`.
   */
  implicit def EitherHash[A: Hash, B: Hash]: Hash[Either[A, B]] =
    Hash[A] either Hash[B]

  /**
   * Hashing for `Float` values.
   */
  implicit val FloatHash: Hash[Float] =
    default

  /**
   * Hashing for `Int` values.
   */
  implicit val IntHash: Hash[Int] =
    default

  /**
   * Derives a `Hash[List[A]]` given a `Hash[A]`.
   */
  implicit def ListHash[A: Hash]: Hash[List[A]] =
    make(_.map(Hash[A].hash).hashCode, _.corresponds(_)(_ === _))

  /**
   * Hashing for `Long` values.
   */
  implicit val LongHash: Hash[Long] =
    default

  /**
   * Derives a `Hash[Map[A, B]]` given a `Hash[B]`. Due to the limitations of
   * Scala's `Map`, this uses object equality and hash code on the keys.
   */
  implicit def MapHash[A, B: Hash]: Hash[Map[A, B]] =
    make(
      _.transform((_, v) => v.hash).hashCode,
      (map1, map2) =>
        map1.size == map2.size &&
          map1.forall { case (key, value) => map2.get(key).fold(false)(_ === value) }
    )

  /**
   * Hashing for `Nothing` values. Note that since there are not values of type
   * `Nothing` the `hash` method of this instance can never be called but it
   * can be useful in deriving instances for more complex types.
   */
  implicit val NothingHash: Hash[Nothing] =
    default

  /**
   * Derives a `Hash[Option[A]]` given a `Hash[A]`.
   */
  implicit def OptionHash[A: Hash]: Hash[Option[A]] =
    make(
      _.map(_.hash).hashCode, {
        case (None, None)         => true
        case (Some(a1), Some(a2)) => a1 === a2
        case _                    => false
      }
    )

  /**
   * Hashing for `Set[A]` values. Due to the limitations of Scala's `Set`,
   * this uses object equality and hash code on the elements.
   */
  implicit def SetHash[A]: Hash[Set[A]] =
    default

  /**
   * Hashing for `String` values.
   */
  implicit val StringHash: Hash[String] =
    default

  /**
   * Derives a `Hash` for a product type given a `Hash` for each element of the
   * product type.
   */
  implicit def Tuple2Hash[A: Hash, B: Hash]: Hash[(A, B)] =
    Hash[A] both Hash[B]

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple3Hash[A: Hash, B: Hash, C: Hash]: Hash[(A, B, C)] =
    make(
      { case (a, b, c)                    => (a.hash, b.hash, c.hash).hashCode },
      { case ((a1, b1, c1), (a2, b2, c2)) => a1 === a2 && b1 === b2 && c1 === c2 }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple4Hash[A: Hash, B: Hash, C: Hash, D: Hash]: Hash[(A, B, C, D)] =
    make(
      { case (a, b, c, d)                         => (a.hash, b.hash, c.hash, d.hash).hashCode },
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple5Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash]: Hash[(A, B, C, D, E)] =
    make(
      { case (a, b, c, d, e) => (a.hash, b.hash, c.hash, d.hash, e.hash).hashCode }, {
        case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple6Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash]: Hash[(A, B, C, D, E, F)] =
    make(
      { case (a, b, c, d, e, f) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash).hashCode }, {
        case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple7Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash]: Hash[(A, B, C, D, E, F, G)] =
    make(
      { case (a, b, c, d, e, f, g) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash).hashCode }, {
        case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple8Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash, H: Hash]
    : Hash[(A, B, C, D, E, F, G, H)] =
    make(
      { case (a, b, c, d, e, f, g, h) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash).hashCode }, {
        case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple9Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash, H: Hash, I: Hash]
    : Hash[(A, B, C, D, E, F, G, H, I)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i) =>
          (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash).hashCode
      }, {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple10Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j) =>
          (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash).hashCode
      }, {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple11Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k) =>
          (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash).hashCode
      }, {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple12Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l) =>
          (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash, l.hash).hashCode
      }, {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple13Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
          (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash, l.hash, m.hash).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple14Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple15Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple16Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple17Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple18Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash,
    R: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash,
            r.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple19Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash,
    R: Hash,
    S: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash,
            r.hash,
            s.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple20Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash,
    R: Hash,
    S: Hash,
    T: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash,
            r.hash,
            s.hash,
            t.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple21Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash,
    R: Hash,
    S: Hash,
    T: Hash,
    U: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash,
            r.hash,
            s.hash,
            t.hash,
            u.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2 && u1 === u2
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple22Hash[
    A: Hash,
    B: Hash,
    C: Hash,
    D: Hash,
    E: Hash,
    F: Hash,
    G: Hash,
    H: Hash,
    I: Hash,
    J: Hash,
    K: Hash,
    L: Hash,
    M: Hash,
    N: Hash,
    O: Hash,
    P: Hash,
    Q: Hash,
    R: Hash,
    S: Hash,
    T: Hash,
    U: Hash,
    V: Hash
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    make(
      {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
          (
            a.hash,
            b.hash,
            c.hash,
            d.hash,
            e.hash,
            f.hash,
            g.hash,
            h.hash,
            i.hash,
            j.hash,
            k.hash,
            l.hash,
            m.hash,
            n.hash,
            o.hash,
            p.hash,
            q.hash,
            r.hash,
            s.hash,
            t.hash,
            u.hash,
            v.hash
          ).hashCode
      }, {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2 && u1 === u2 && v1 === v2
      }
    )

  /**
   * Hashing for `Unit` values. Since there is only one `Unit` value all values
   * have the same hash.
   */
  implicit val UnitHash: Hash[Unit] =
    default

  /**
   * Derives a `Hash[Vector[A]]` given a `Hash[A]`.
   */
  implicit def VectorHash[A: Hash]: Hash[Vector[A]] =
    make(_.map(_.hash).hashCode, _.corresponds(_)(_ === _))
}

trait HashSyntax {

  /**
   * Provides infix syntax for hashing a value.
   */
  implicit class HashOps[A](a: A) {

    /**
     * Returns the hash of this value.
     */
    def hash(implicit hash: Hash[A]): Int =
      hash.hash(a)

    /**
     * A symbolic alias for `hash`.
     */
    def ##(implicit hash: Hash[A]): Int =
      hash.hash(a)
  }
}
