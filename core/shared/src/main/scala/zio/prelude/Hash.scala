/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

import zio.{Chunk, NonEmptyChunk}

import scala.annotation.implicitNotFound

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
   * Constructs a `Hash[B]` given a `Hash[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and hash the `A` values.
   */
  override def contramap[B](f: B => A): Hash[B] =
    Hash.make(
      b => hash(f(b)),
      (b1, b2) => equal(f(b1), f(b2))
    )
}

object Hash {

  /**
   * The contravariant instance for `Hash`.
   */
  implicit val HashContravariant: Contravariant[Hash] =
    new Contravariant[Hash] {
      def contramap[A, B](f: B => A): Hash[A] => Hash[B] =
        _.contramap(f)
    }

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
   * Constructs an instance from a hash function and an `Equal` instance.
   */
  def makeFrom[A](hash0: A => Int, equal0: Equal[A]): Hash[A] =
    make(hash0, (l, r) => equal0.equal(l, r))

  /**
   * Constructs a `Hash[A]` that uses the default notion of hashing embodied in
   * the implementation of `hashCode` for values of type `A`.
   */
  def default[A]: Hash[A] =
    DefaultHash

  /**
   * Derives a `Hash[Chunk[A]]` given a `Hash[A]`.
   */
  implicit def ChunkHash[A: Hash]: Hash[Chunk[A]] =
    makeFrom(_.map(_.hash).hashCode, Equal.ChunkEqual)

  /**
   * Derives a `Hash[F[A]]` given a `Derive[F, Hash]` and a `Hash[A]`.
   */
  implicit def DeriveHash[F[_], A](implicit derive: Derive[F, Hash], hash: Hash[A]): Hash[F[A]] =
    derive.derive(hash)

  /**
   * Derives a `Hash[Either[A, B]]` given a `Hash[A]` and a `Hash[B]`.
   */
  implicit def EitherHash[A: Hash, B: Hash]: Hash[Either[A, B]] =
    makeFrom(
      {
        case Left(a)  => Left(a.hash).hashCode
        case Right(b) => Right(b.hash).hashCode
      },
      Equal.EitherEqual
    )

  /**
   * Derives a `Hash[List[A]]` given a `Hash[A]`.
   */
  implicit def ListHash[A: Hash]: Hash[List[A]] =
    makeFrom(_.map(Hash[A].hash).hashCode, Equal.ListEqual)

  /**
   * Derives a `Hash[Map[A, B]]` given a `Hash[B]`. Due to the limitations of
   * Scala's `Map`, this uses object equality and hash code on the keys.
   */
  implicit def MapHash[A, B: Hash]: Hash[Map[A, B]] =
    makeFrom(_.transform((_, v) => v.hash).hashCode, Equal.MapPartialOrd)

  /**
   * Derives a `Hash[NonEmptyChunk[A]]` given a `Hash[A]`.
   */
  implicit def NonEmptyChunkHash[A: Hash]: Hash[NonEmptyChunk[A]] =
    Hash[Chunk[A]].contramap(_.toChunk)

  /**
   * Derives a `Hash[Option[A]]` given a `Hash[A]`.
   */
  implicit def OptionHash[A: Hash]: Hash[Option[A]] =
    makeFrom(_.map(_.hash).hashCode, Equal.OptionEqual)

  /**
   * Derives a `Hash` for a product type given a `Hash` for each element of the
   * product type.
   */
  implicit def Tuple2Hash[A: Hash, B: Hash]: Hash[(A, B)] =
    makeFrom({ case (a, b) => (a.hash, b.hash).hashCode }, Equal.Tuple2Equal)

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple3Hash[A: Hash, B: Hash, C: Hash]: Hash[(A, B, C)] =
    makeFrom({ case (a, b, c) => (a.hash, b.hash, c.hash).hashCode }, Equal.Tuple3Equal)

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple4Hash[A: Hash, B: Hash, C: Hash, D: Hash]: Hash[(A, B, C, D)] =
    makeFrom({ case (a, b, c, d) => (a.hash, b.hash, c.hash, d.hash).hashCode }, Equal.Tuple4Equal)

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple5Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash]: Hash[(A, B, C, D, E)] =
    makeFrom(
      { case (a, b, c, d, e) => (a.hash, b.hash, c.hash, d.hash, e.hash).hashCode },
      Equal.Tuple5Equal
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple6Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash]: Hash[(A, B, C, D, E, F)] =
    makeFrom(
      { case (a, b, c, d, e, f) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash).hashCode },
      Equal.Tuple6Equal
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple7Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash]: Hash[(A, B, C, D, E, F, G)] =
    makeFrom(
      { case (a, b, c, d, e, f, g) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash).hashCode },
      Equal.Tuple7Equal
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple8Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash, H: Hash]
    : Hash[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      { case (a, b, c, d, e, f, g, h) => (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash).hashCode },
      Equal.Tuple8Equal
    )

  /**
   * Derives an `Hash` for a product type given an `Hash` for each element of
   * the product type.
   */
  implicit def Tuple9Hash[A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash, H: Hash, I: Hash]
    : Hash[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      { case (a, b, c, d, e, f, g, h, i) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash).hashCode
      },
      Equal.Tuple9Equal
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
    makeFrom(
      { case (a, b, c, d, e, f, g, h, i, j) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash).hashCode
      },
      Equal.Tuple10Equal
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
    makeFrom(
      { case (a, b, c, d, e, f, g, h, i, j, k) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash).hashCode
      },
      Equal.Tuple11Equal
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
    makeFrom(
      { case (a, b, c, d, e, f, g, h, i, j, k, l) =>
        (a.hash, b.hash, c.hash, d.hash, e.hash, f.hash, g.hash, h.hash, i.hash, j.hash, k.hash, l.hash).hashCode
      },
      Equal.Tuple12Equal
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
    makeFrom(
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
      },
      Equal.Tuple13Equal
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
    makeFrom(
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
      },
      Equal.Tuple14Equal
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
    makeFrom(
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
      },
      Equal.Tuple15Equal
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
    makeFrom(
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
      },
      Equal.Tuple16Equal
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
    makeFrom(
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
      },
      Equal.Tuple17Equal
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
    makeFrom(
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
      },
      Equal.Tuple18Equal
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
    makeFrom(
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
      },
      Equal.Tuple19Equal
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
    makeFrom(
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
      },
      Equal.Tuple20Equal
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
    makeFrom(
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
      },
      Equal.Tuple21Equal
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
    makeFrom(
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
      },
      Equal.Tuple22Equal
    )

  /**
   * Derives a `Hash[Vector[A]]` given a `Hash[A]`.
   */
  implicit def VectorHash[A: Hash]: Hash[Vector[A]] =
    makeFrom(_.map(_.hash).hashCode, Equal.VectorEqual)

  /**
   * A `Hash` instance for `Any` values that uses Scala's default notions of
   * equality and hashing embodied in `equals` and `hashCode`.
   */
  private lazy val DefaultHash: Hash[Any] =
    Hash.make(_.##, _ == _)
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
