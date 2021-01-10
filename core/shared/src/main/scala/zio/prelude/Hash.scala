package zio.prelude

import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}
import zio.{Chunk, NonEmptyChunk}

import scala.annotation.implicitNotFound
import scala.{math => sm}
import zio.Fiber

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` can be hashed.
 */
@implicitNotFound("No implicit Hash defined for ${A}.")
abstract class Hash[-A](implicit E: Equal[A]) { self =>
  //// Exported members
  final def equal(l: A, r: A): Boolean =
    E.equal(l, r)

  final def notEqual(l: A, r: A): Boolean =
    E.notEqual(l, r)

  def toScala[A1 <: A]: sm.Equiv[A1] = self.equal(_, _)
  ////

  /**
   * Returns the hash of the specified value.
   */
  def hash(a: A): Int

  /**
   * Constructs a `Hash[(A, B)]` given a `Hash[A]` and `Hash[B]` by hashing the
   * `A` and `B` values together.
   */
  final def both[B: Equal](that: Hash[B]): Hash[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs a `Hash[C]` given a `Hash[A]`, a `Hash[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, and hash the `A` and `B` values together.
   */
  final def bothWith[B: Equal, C](that: Hash[B])(f: C => (A, B)): Hash[C] =
    Hash.make[C](c =>
      f(c) match {
        case (a, b) => (self.hash(a), that.hash(b)).hashCode
      }
    )(Equal[(A, B)].contramap(f))

  /**
   * Constructs a `Hash[B]` given a `Hash[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and hash the `A` values.
   */
  final def contramap[B](f: B => A): Hash[B] =
    Hash.make[B](b => hash(f(b)))(Equal[A].contramap(f))

  /**
   * Constructs a `Hash[Either[A, B]]` given a `Hash[A]` and a `Hash[B]`. The
   * instance will hash either the `A` or `B` values.
   */
  final def either[B: Equal](that: Hash[B]): Hash[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs a `Hash[C]` given a `Hash[A]`, a `Hash[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]` and then hash either the `A` or `B`
   * values.
   */
  final def eitherWith[B: Equal, C](that: Hash[B])(f: C => Either[A, B]): Hash[C] =
    Hash.make[C](c =>
      f(c) match {
        case Left(a)  => Left(self.hash(a)).hashCode
        case Right(b) => Right(that.hash(b)).hashCode
      }
    )(Equal[Either[A, B]].contramap(f))
}

object Hash extends Lawful[Hash] {

  /**
   * For all values `a1` and `a2`, if `a1` is equal to `a2` then the hash of
   * `a1` is equal to the hash of `a2`.
   */
  val consistencyLaw: Laws[Hash] =
    new Laws.Law2[Hash]("consistencyLaw") {
      def apply[A: Equal](a1: A, a2: A)(implicit caps: Hash[A]): TestResult =
        (a1 <-> a2) ==> (Hash[A].hash(a1) <-> Hash[A].hash(a2))
    }

  /**
   * The set of all laws that instances of `Hash` must satisfy.
   */
  val laws: Laws[Hash] =
    consistencyLaw

  /**
   * The contravariant instance for `Hash`.
   */
  implicit val HashContravariant: Contravariant[Hash] =
    new Contravariant[Hash] {
      def contramap[A, B: Equal](f: B => A): Hash[A] => Hash[B] =
        _.contramap(f)
    }

  /**
   * The `IdentityBoth` (and thus `AssociativeBoth`) instance for `Hash`.
   */
  // implicit val HashIdentityBoth: IdentityBoth[Hash] =
  //   new IdentityBoth[Hash] {
  //     val any: Hash[Any] =
  //       AnyHash

  //     def both[A, B](fa: => Hash[A], fb: => Hash[B]): Hash[(A, B)] =
  //       fa.both(fb)
  //   }

  /**
   * The `IdentityEither` (and thus `AssociativeEither`) instance for `Hash`.
   */
  implicit val HashIdentityEither: IdentityEither[Hash] =
    new IdentityEither[Hash] {
      def either[A, B: Equal](fa: => Hash[A], fb: => Hash[B]): Hash[Either[A, B]] =
        fa.either(fb)

      val none: Hash[Nothing] =
        AnyHash
    }

  /**
   * Summons an implicit `Hash[A]`.
   */
  def apply[A](implicit hash: Hash[A]): Hash[A] =
    hash

  /**
   * Constructs an instance from a function.
   */
  def make[A: Equal](hash0: A => Int): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int = hash0(a)
    }

  /**
   * Constructs a `Hash[A]` that uses the default notion of hashing embodied in
   * the implementation of `hashCode` for values of type `A`.
   */
  def default[A: Equal]: Hash[A] =
    make(_.hashCode())

  val AnyHash: Hash[Any] =
    Hash.make((_: Any) => 0)(Equal.AnyEqual)

  implicit val NothingHash: Hash[Nothing] =
    Hash
      .make[Nothing]((_: Nothing) => sys.error("nothing.hash"))

  implicit val BooleanHash: Hash[Boolean] =
    Hash.default

  implicit val ByteHash: Hash[Byte] =
    Hash.default

  implicit val ShortHash: Hash[Short] =
    Hash.default

  implicit val CharHash: Hash[Char] =
    Hash.default

  implicit val ClassHash: Hash[Class[_]] =
    Hash.default

  implicit val DoubleHash: Hash[Double] =
    Hash.make(_.hashCode)

  implicit val FloatHash: Hash[Float] =
    Hash.make(_.hashCode)

  implicit lazy val FiberIdHash: Hash[Fiber.Id] =
    Hash[(Long, Long)].contramap[Fiber.Id](fid => (fid.startTimeMillis, fid.seqNumber))

  implicit val IntHash: Hash[Int] =
    Hash.default

  implicit val LongHash: Hash[Long] =
    Hash.default

  implicit def SetHash[A]: Hash[Set[A]] =
    Hash.make(_.hashCode)

  implicit val StringHash: Hash[String] =
    Hash.default

  lazy val ThrowableHash: Hash[Throwable] = {
    implicit val ThrowableEqual: Equal[Throwable] = implicitly
    implicit val hashOT: Hash[Option[Throwable]]  =
      Hash.OptionHash(
        // use an indirect instance, so that calling ThrowableHash infinitely doesn't cause stack overflow
        new Hash[Throwable] {
          def hash(a: Throwable): Int = ThrowableHash.hash(a)
        },
        ThrowableEqual
      )

    Hash[(Class[_], String, Option[Throwable])].contramap { t =>
      (t.getClass, t.getMessage, Option(t.getCause))
    }
  }

  /**
   * Derives a `Hash[Chunk[A]]` given a `Hash[A]`.
   */
  implicit def ChunkHash[A: Hash: Equal]: Hash[Chunk[A]] =
    make(_.map(_.hash).hashCode)

  /**
   * Derives a `Hash[F[A]]` given a `Derive[F, Hash]` and a `Hash[A]`.
   */
  implicit def DeriveHash[F[_], A](implicit derive: Derive[F, Hash], hash: Hash[A]): Hash[F[A]] =
    derive.derive(hash)

  /**
   * Derives a `Hash[Either[A, B]]` given a `Hash[A]` and a `Hash[B]`.
   */
  implicit def EitherHash[A: Hash: Equal, B: Hash: Equal]: Hash[Either[A, B]] =
    make(
      {
        case Left(a)  => Left(a.hash).hashCode
        case Right(b) => Right(b.hash).hashCode
      }
    )

  /**
   * Derives a `Hash[List[A]]` given a `Hash[A]`.
   */
  implicit def ListHash[A: Hash: Equal]: Hash[List[A]] =
    make(_.map(Hash[A].hash).hashCode)

  /**
   * Derives a `Hash[Map[A, B]]` given a `Hash[B]`. Due to the limitations of
   * Scala's `Map`, this uses object equality and hash code on the keys.
   */
  implicit def MapHash[A, B: Hash: Equal]: Hash[Map[A, B]] =
    make(_.transform((_, v) => v.hash).hashCode)

  /**
   * Derives a `Hash[NonEmptyChunk[A]]` given a `Hash[A]`.
   */
  implicit def NonEmptyChunkHash[A: Hash: Equal]: Hash[NonEmptyChunk[A]] =
    Hash[Chunk[A]].contramap(_.toChunk)

  /**
   * Derives a `Hash[Option[A]]` given a `Hash[A]`.
   */
  implicit def OptionHash[A: Hash: Equal]: Hash[Option[A]] =
    make(_.map(_.hash).hashCode)

  /**
   * Derives a `Hash` for a product type given a `Hash` for each element of the
   * product type.
   */
  implicit def Tuple2Hash[A: Hash: Equal, B: Hash: Equal]: Hash[(A, B)]                                       =
    make({ case (a, b) => (a.hash, b.hash).hashCode })

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple3Hash[A: Hash: Equal, B: Hash: Equal, C: Hash: Equal]: Hash[(A, B, C)]                    =
    make({ case (a, b, c) => (a.hash, b.hash, c.hash).hashCode })

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple4Hash[A: Hash: Equal, B: Hash: Equal, C: Hash: Equal, D: Hash: Equal]: Hash[(A, B, C, D)] =
    make({ case (a, b, c, d) => (a.hash, b.hash, c.hash, d.hash).hashCode })

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple5Hash[A: Hash: Equal, B: Hash: Equal, C: Hash: Equal, D: Hash: Equal, E: Hash: Equal]
    : Hash[(A, B, C, D, E)]                                                                                   =
    make(
      { case (a, b, c, d, e) => (a.hash, b.hash, c.hash, d.hash, e.hash).hashCode }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple6Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal
  ]: Hash[(A, B, C, D, E, F)] =
    make(
      { case (a, b, c, d, e, f) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash).hashCode }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple7Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G)] =
    make(
      { case (a, b, c, d, e, f, g) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash).hashCode }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple8Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H)] =
    make(
      { case (a, b, c, d, e, f, g, h) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash).hashCode }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple9Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I)] =
    make(
      { case (a, b, c, d, e, f, g, h, i) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash).hashCode
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple10Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash).hashCode
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple11Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash).hashCode
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple12Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash, l.hash).hashCode
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple13Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
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
          m.hash
        ).hashCode
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple14Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple15Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple16Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple17Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple18Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal,
    R: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple19Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal,
    R: Hash: Equal,
    S: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple20Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal,
    R: Hash: Equal,
    S: Hash: Equal,
    T: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple21Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal,
    R: Hash: Equal,
    S: Hash: Equal,
    T: Hash: Equal,
    U: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
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
      }
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple22Hash[
    A: Hash: Equal,
    B: Hash: Equal,
    C: Hash: Equal,
    D: Hash: Equal,
    E: Hash: Equal,
    F: Hash: Equal,
    G: Hash: Equal,
    H: Hash: Equal,
    I: Hash: Equal,
    J: Hash: Equal,
    K: Hash: Equal,
    L: Hash: Equal,
    M: Hash: Equal,
    N: Hash: Equal,
    O: Hash: Equal,
    P: Hash: Equal,
    Q: Hash: Equal,
    R: Hash: Equal,
    S: Hash: Equal,
    T: Hash: Equal,
    U: Hash: Equal,
    V: Hash: Equal
  ]: Hash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    make(
      { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
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
      }
    )

  /**
   * Derives a `Hash[Vector[A]]` given a `Hash[A]`.
   */
  implicit def VectorHash[A: Hash: Equal]: Hash[Vector[A]] =
    make(_.map(_.hash).hashCode)
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
