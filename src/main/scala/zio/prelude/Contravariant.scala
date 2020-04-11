package zio.prelude

import zio.{ Schedule, ZIO, ZLayer, ZManaged, ZQueue, ZRef }
import zio.stream.{ ZSink, ZStream }

/**
 * `Contravariant[F]` provides implicit evidence that `F[-_]` is a
 * contravariant enfofunctor in the category of Scala objects.
 *
 * `Contravariant` instances of type `F[A]` "consume" values of type `A` in
 * some sense. For example, `Equal[A]` takes two values of type `A` as input
 * and returns a `Boolean` indicating whether they are equal. Similarly, a
 * `Ord[A]` takes two values of type `A` as input and returns an `Ordering`
 * with the result of comparing them and `Hash` takes an `A` value and returns
 * an `Int`.
 *
 * Common examples of contravariant instances in ZIO include effects with
 * regard to their environment types, sinks with regard to their input type,
 * and polymorphic queues and references regarding their input types.
 *
 * `Contravariant` instances support a `contramap` operation, which allows
 * transforming the input type given a function from the new input type to the
 * old input type. For example, if we have an `Ord[Int]` that allows us to
 * compare two integers and we have a function `String => Int` that returns
 * the length of a string, then we can construct an `Ord[String]` that
 * compares strings by computing their lengths with the provided function and
 * comparing those.
 */
trait Contravariant[F[-_]] {

  /**
   * Lift a function from `A` to `B` to a function from `F[B]` to `F[A]`.
   */
  def contramap[A, B](f: B => A): F[A] => F[B]

  /**
   * Contramapping with the identity function must be an identity function.
   */
  def identityLaw[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    contramap(identity[A](_))(fa) === fa

  /**
   * Contramapping by `f` followed by `g` must be the same as contramapping
   * with the composition of `f` and `g`.
   */
  def compositionLaw[A, B, C](fa: F[A], f: C => B, g: B => A)(implicit equal: Equal[F[C]]): Boolean =
    contramap(f)(contramap(g)(fa)) === contramap(f andThen g)(fa)
}
object Contravariant {

  /**
   * Summons an implicit `Contravariant[F]`.
   */
  def apply[F[-_]](implicit contravariant: Contravariant[F]): Contravariant[F] =
    contravariant

  /**
   * The contravariant instance for `Function1`.
   */
  implicit def Function1Contravariant[A]: Contravariant[({ type f[-x] = x => A })#f] =
    new Contravariant[({ type f[-x] = x => A })#f] {
      def contramap[X, Y](function: Y => X): (X => A) => (Y => A) =
        apply => x => apply(function(x))
    }

  /**
   * The contravariant instance for `Function2`.
   */
  implicit def Function2Contravariant[A, B]: Contravariant[({ type f[-x] = (x, A) => B })#f] =
    new Contravariant[({ type f[-x] = (x, A) => B })#f] {
      def contramap[X, Y](function: Y => X): ((X, A) => B) => ((Y, A) => B) =
        apply => (x, a) => apply(function(x), a)
    }

  /**
   * The contravariant instance for `Function3`.
   */
  implicit def Function3Contravariant[A, B, C]: Contravariant[({ type f[-x] = (x, A, B) => C })#f] =
    new Contravariant[({ type f[-x] = (x, A, B) => C })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B) => C) => ((Y, A, B) => C) =
        apply => (x, a, b) => apply(function(x), a, b)
    }

  /**
   * The contravariant instance for `Function4`.
   */
  implicit def Function4Contravariant[A, B, C, D]: Contravariant[({ type f[-x] = (x, A, B, C) => D })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C) => D })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C) => D) => ((Y, A, B, C) => D) =
        apply => (x, a, b, c) => apply(function(x), a, b, c)
    }

  /**
   * The contravariant instance for `Function5`.
   */
  implicit def Function5Contravariant[A, B, C, D, E]: Contravariant[({ type f[-x] = (x, A, B, C, D) => E })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D) => E })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C, D) => E) => ((Y, A, B, C, D) => E) =
        apply => (x, a, b, c, d) => apply(function(x), a, b, c, d)
    }

  /**
   * The contravariant instance for `Function6`.
   */
  implicit def Function6Contravariant[A, B, C, D, E, F]: Contravariant[({ type f[-x] = (x, A, B, C, D, E) => F })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E) => F })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C, D, E) => F) => ((Y, A, B, C, D, E) => F) =
        apply => (x, a, b, c, d, e) => apply(function(x), a, b, c, d, e)
    }

  /**
   * The contravariant instance for `Function7`.
   */
  implicit def Function7Contravariant[A, B, C, D, E, F, G]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F) => G })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F) => G })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C, D, E, F) => G) => ((Y, A, B, C, D, E, F) => G) =
        apply => (x, a, b, c, d, e, f) => apply(function(x), a, b, c, d, e, f)
    }

  /**
   * The contravariant instance for `Function8`.
   */
  implicit def Function8Contravariant[A, B, C, D, E, F, G, H]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G) => H })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G) => H })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C, D, E, F, G) => H) => ((Y, A, B, C, D, E, F, G) => H) =
        apply => (x, a, b, c, d, e, f, g) => apply(function(x), a, b, c, d, e, f, g)
    }

  /**
   * The contravariant instance for `Function9`.
   */
  implicit def Function9Contravariant[A, B, C, D, E, F, G, H, I]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H) => I })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H) => I })#f] {
      def contramap[X, Y](function: Y => X): ((X, A, B, C, D, E, F, G, H) => I) => ((Y, A, B, C, D, E, F, G, H) => I) =
        apply => (x, a, b, c, d, e, f, g, h) => apply(function(x), a, b, c, d, e, f, g, h)
    }

  /**
   * The contravariant instance for `Function10`.
   */
  implicit def Function10Contravariant[A, B, C, D, E, F, G, H, I, J]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I) => J })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I) => J })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I) => J) => ((Y, A, B, C, D, E, F, G, H, I) => J) =
        apply => (x, a, b, c, d, e, f, g, h, i) => apply(function(x), a, b, c, d, e, f, g, h, i)
    }

  /**
   * The contravariant instance for `Function11`.
   */
  implicit def Function11Contravariant[A, B, C, D, E, F, G, H, I, J, K]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J) => K })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J) => K })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I, J) => K) => ((Y, A, B, C, D, E, F, G, H, I, J) => K) =
        apply => (x, a, b, c, d, e, f, g, h, i, j) => apply(function(x), a, b, c, d, e, f, g, h, i, j)
    }

  /**
   * The contravariant instance for `Function12`.
   */
  implicit def Function12Contravariant[A, B, C, D, E, F, G, H, I, J, K, L]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K) => L })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K) => L })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I, J, K) => L) => ((Y, A, B, C, D, E, F, G, H, I, J, K) => L) =
        apply => (x, a, b, c, d, e, f, g, h, i, j, k) => apply(function(x), a, b, c, d, e, f, g, h, i, j, k)
    }

  /**
   * The contravariant instance for `Function13`.
   */
  implicit def Function13Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L) => M })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L) => M })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I, J, K, L) => M) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L) => M) =
        apply => (x, a, b, c, d, e, f, g, h, i, j, k, l) => apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l)
    }

  /**
   * The contravariant instance for `Function14`.
   */
  implicit def Function14Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M) => N })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M) => N })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I, J, K, L, M) => N) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M) => N) =
        apply => (x, a, b, c, d, e, f, g, h, i, j, k, l, m) => apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m)
    }

  /**
   * The contravariant instance for `Function15`.
   */
  implicit def Function15Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#f] {
      def contramap[X, Y](
        function: Y => X
      ): ((X, A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n) => apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    }

  /**
   * The contravariant instance for `Function16`.
   */
  implicit def Function16Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }

  /**
   * The contravariant instance for `Function17`.
   */
  implicit def Function17Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }

  /**
   * The contravariant instance for `Function18`.
   */
  implicit def Function18Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }

  /**
   * The contravariant instance for `Function10`.
   */
  implicit def Function19Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    }

  /**
   * The contravariant instance for `Function20`.
   */
  implicit def Function20Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    }

  /**
   * The contravariant instance for `Function21`.
   */
  implicit def Function21Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    }

  /**
   * The contravariant instance for `Function22`.
   */
  implicit def Function22Contravariant[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
    : Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#f] =
    new Contravariant[({ type f[-x] = (x, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#f] {
      def contramap[X, Y](function: Y => X): (
        (X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
      ) => ((Y, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) =
        apply =>
          (x, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
            apply(function(x), a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    }

  /**
   * The contravariant instance for `Schedule`.
   */
  implicit def ScheduleContravariant[R, B]: Contravariant[({ type f[-x] = Schedule[R, x, B] })#f] =
    new Contravariant[({ type f[-x] = Schedule[R, x, B] })#f] {
      def contramap[A, A0](f: A0 => A): Schedule[R, A, B] => Schedule[R, A0, B] =
        schedule => schedule.contramap(f)
    }

  /**
   * The contravariant instance for `ZIO`.
   */
  implicit def ZIOContravariant[E, A]: Contravariant[({ type f[-x] = ZIO[x, E, A] })#f] =
    new Contravariant[({ type f[-x] = ZIO[x, E, A] })#f] {
      def contramap[R, R0](f: R0 => R): ZIO[R, E, A] => ZIO[R0, E, A] =
        zio => zio.provideSome(f)
    }

  /**
   * The contravariant instance for `ZLayer`.
   */
  implicit def ZLayerContravariant[E, ROut]: Contravariant[({ type f[-x] = ZLayer[x, E, ROut] })#f] =
    new Contravariant[({ type f[-x] = ZLayer[x, E, ROut] })#f] {
      def contramap[RIn, RIn0](f: RIn0 => RIn): ZLayer[RIn, E, ROut] => ZLayer[RIn0, E, ROut] =
        layer => ZLayer.fromFunctionMany(f) >>> layer
    }

  /**
   * The contravariant instance for `ZManaged`.
   */
  implicit def ZManagedContravariant[E, A]: Contravariant[({ type f[-x] = ZManaged[x, E, A] })#f] =
    new Contravariant[({ type f[-x] = ZManaged[x, E, A] })#f] {
      def contramap[R, R0](f: R0 => R): ZManaged[R, E, A] => ZManaged[R0, E, A] =
        managed => managed.provideSome(f)
    }

  /**
   * The contravariant instance for `ZQueue`.
   */
  implicit def ZQueueContravariant[RA, EA, RB, EB, A, B]
    : Contravariant[({ type f[-x] = ZQueue[RA, EA, RB, EB, x, B] })#f] =
    new Contravariant[({ type f[-x] = ZQueue[RA, EA, RB, EB, x, B] })#f] {
      def contramap[A, C](f: C => A): ZQueue[RA, EA, RB, EB, A, B] => ZQueue[RA, EA, RB, EB, C, B] =
        queue => queue.contramap(f)
    }

  /**
   * The contravariant instance for `ZRef`.
   */
  implicit def ZRefContravariant[EA, EB, B]: Contravariant[({ type f[-x] = ZRef[EA, EB, x, B] })#f] =
    new Contravariant[({ type f[-x] = ZRef[EA, EB, x, B] })#f] {
      def contramap[A, C](f: C => A): ZRef[EA, EB, A, B] => ZRef[EA, EB, C, B] =
        ref => ref.contramap(f)
    }

  /**
   * The contravariant instance for `ZSink`.
   */
  implicit def ZSinkContravariant[R, E, A0, B]: Contravariant[({ type f[-x] = ZSink[R, E, A0, x, B] })#f] =
    new Contravariant[({ type f[-x] = ZSink[R, E, A0, x, B] })#f] {
      def contramap[A, C](f: C => A): ZSink[R, E, A0, A, B] => ZSink[R, E, A0, C, B] =
        sink => sink.contramap(f)
    }

  /**
   * The contravariant instance for `ZStream`.
   */
  implicit def ZStreamContravariant[E, A]: Contravariant[({ type f[-x] = ZStream[x, E, A] })#f] =
    new Contravariant[({ type f[-x] = ZStream[x, E, A] })#f] {
      def contramap[R, R0](f: R0 => R): ZStream[R, E, A] => ZStream[R0, E, A] =
        stream => stream.provideSome(f)
    }
}
