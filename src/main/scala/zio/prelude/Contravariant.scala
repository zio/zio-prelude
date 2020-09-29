package zio.prelude

import zio.prelude.coherent.ContravariantDeriveEqual
import zio.stream.{ ZSink, ZStream }
import zio.test.TestResult
import zio.test.laws._
import zio.{ Schedule, ZIO, ZLayer, ZManaged, ZQueue, ZRef, ZRefM }

trait ContravariantSubset[F[-_], Subset[_]] {
  def contramapSubset[A, B: Subset](f: B => A): F[A] => F[B]
}

/**
 * `Contravariant[F]` provides implicit evidence that `F[-_]` is a
 * contravariant endofunctor in the category of Scala objects.
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
trait ContravariantInstance[F[-_]] extends Contravariant[F] with ContravariantSubset[F, AnyType] with Invariant[F] {
  final def contramapSubset[A, A1: AnyType](f: A1 => A): F[A] => F[A1] =
    contramap(f)

  final def invmap[A, A1](f: A <=> A1): F[A] <=> F[A1] =
    Equivalence((fa: F[A]) => contramap(f.from)(fa), (fb: F[A1]) => contramap(f.to)(fb))

  override def contramap[R, E, A, R1](r: R1 => R): F[R] => F[R1] = contramap(r)
}

trait ContravariantLeftInstance[F[-_]] extends Contravariant[F] with ContravariantSubset[F, AnyType] with Invariant[F] {
  final def contramapSubset[A, A1: AnyType](f: A1 => A): F[A] => F[A1] =
    contramap(f)

  final def invmap[A, A1](f: A <=> A1): F[A] <=> F[A1] =
    Equivalence((fa: F[A]) => contramap(f.from)(fa), (fb: F[A1]) => contramap(f.to)(fb))

  override def contramap[R, E, A, R1](r: R1 => R): F[R] => F[R1] = contramap(r)
}

object Contravariant extends LawfulF.Contravariant[ContravariantDeriveEqual, Equal] {

  /**
   * Contramapping with the identity function must not change the structure.
   */
  val identityLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.Law1[ContravariantDeriveEqual, Equal]("identityLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.contramap(identity[A]) <-> fa
    }

  /**
   * Contramapping by `f` followed by `g` must be the same as contramapping
   * with the composition of `f` and `g`.
   */
  val compositionLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.ComposeLaw[ContravariantDeriveEqual, Equal]("compositionLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        f: B => A,
        g: C => B
      ): TestResult = {
        // Dotty can't infer this https://github.com/zio/zio-prelude/issues/273
        implicit val equalFC: Equal[F[C]] = Equal.DeriveEqual[F, C]
        fa.contramap(f).contramap(g) <-> fa.contramap(f compose g)
      }
    }

  /**
   * The set of all laws that instances of `Contravariant` must satisfy.
   */
  val laws: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    identityLaw + compositionLaw

  /**
   * Summons an implicit `Contravariant[F]`.
   */
  def apply[F[-_]](implicit contravariant: Contravariant[F]): Contravariant[F] =
    contravariant

  /**
   * The contravariant instance for `Function1[-A, +B] : [*, *] => *`.
   */
  implicit def Function1Contravariant[B]: Contravariant[({ type lambda[-x] = x => B })#lambda] =
    Divariant.Function1Divariant.deriveContravariant

  /**
   * The contravariant instance for `Function2`.
   */
  implicit def Function2Contravariant[B, C]: Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] {

      override def contramap[R, E, A, R1](function: R1 => R): ((R, B) => C) => (R1, B) => C =
        apply => (d, b) => apply(function(d), b)
    }

  /**
   * The contravariant instance for `Function3`.
   */
  implicit def Function3Contravariant[B, C, D]: Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] {

      override def contramap[R, E, A, R1](function: R1 => R): ((R, B, C) => D) => (R1, B, C) => D =
        apply => (e, b, c) => apply(function(e), b, c)
    }

  /**
   * The contravariant instance for `Function4`.
   */
  implicit def Function4Contravariant[B, C, D, E]: Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] {

      override def contramap[R, EE, A, R1](function: R1 => R): ((R, B, C, D) => E) => (R1, B, C, D) => E =
        apply => (f, b, c, d) => apply(function(f), b, c, d)
    }

  /**
   * The contravariant instance for `Function5`.
   */
  implicit def Function5Contravariant[B, C, D, E, F]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] {

      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C, D, E) => F) => (R1, B, C, D, E) => F =
        apply => (g, b, c, d, e) => apply(function(g), b, c, d, e)
    }

  /**
   * The contravariant instance for `Function6`.
   */
  implicit def Function6Contravariant[B, C, D, E, F, G]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] {

      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C, D, E, F) => G) => (R1, B, C, D, E, F) => G =
        apply => (h, b, c, d, e, f) => apply(function(h), b, c, d, e, f)
    }

  /**
   * The contravariant instance for `Function7`.
   */
  implicit def Function7Contravariant[B, C, D, E, F, G, H]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G) => H) => (R1, B, C, D, E, F, G) => H =
        apply => (i, b, c, d, e, f, g) => apply(function(i), b, c, d, e, f, g)
    }

  /**
   * The contravariant instance for `Function8`.
   */
  implicit def Function8Contravariant[B, C, D, E, F, G, H, I]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H) => I) => (R1, B, C, D, E, F, G, H) => I =
        apply => (j, b, c, d, e, f, g, h) => apply(function(j), b, c, d, e, f, g, h)
    }

  /**
   * The contravariant instance for `Function9`.
   */
  implicit def Function9Contravariant[B, C, D, E, F, G, H, I, J]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I) => J) => (R1, B, C, D, E, F, G, H, I) => J =
        apply => (k, b, c, d, e, f, g, h, i) => apply(function(k), b, c, d, e, f, g, h, i)
    }

  /**
   * The contravariant instance for `Function10`.
   */
  implicit def Function10Contravariant[B, C, D, E, F, G, H, I, J, K]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J) => K) => (R1, B, C, D, E, F, G, H, I, J) => K =
        apply => (l, b, c, d, e, f, g, h, i, j) => apply(function(l), b, c, d, e, f, g, h, i, j)
    }

  /**
   * The contravariant instance for `Function11`.
   */
  implicit def Function11Contravariant[B, C, D, E, F, G, H, I, J, K, L]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J, K) => L) => (R1, B, C, D, E, F, G, H, I, J, K) => L =
        apply => (m, b, c, d, e, f, g, h, i, j, k) => apply(function(m), b, c, d, e, f, g, h, i, j, k)
    }

  /**
   * The contravariant instance for `Function12`.
   */
  implicit def Function12Contravariant[B, C, D, E, F, G, H, I, J, K, L, M]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J, K, L) => M) => (R1, B, C, D, E, F, G, H, I, J, K, L) => M =
        apply => (n, b, c, d, e, f, g, h, i, j, k, l) => apply(function(n), b, c, d, e, f, g, h, i, j, k, l)
    }

  /**
   * The contravariant instance for `Function13`.
   */
  implicit def Function13Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J, K, L, M) => N) => (R1, B, C, D, E, F, G, H, I, J, K, L, M) => N =
        apply => (o, b, c, d, e, f, g, h, i, j, k, l, m) => apply(function(o), b, c, d, e, f, g, h, i, j, k, l, m)
    }

  /**
   * The contravariant instance for `Function14`.
   */
  implicit def Function14Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N) => O =
        apply => (p, b, c, d, e, f, g, h, i, j, k, l, m, n) => apply(function(p), b, c, d, e, f, g, h, i, j, k, l, m, n)
    }

  /**
   * The contravariant instance for `Function15`.
   */
  implicit def Function15Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] {

      override def contramap[R, E2, A, R1](
        function: R1 => R
      ): ((R, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P =
        apply =>
          (q, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => apply(function(q), b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }

  /**
   * The contravariant instance for `Function16`.
   */
  implicit def Function16Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] {

      override def contramap[R, E2, A, R1](function: R1 => R): (
        (R, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q =
        apply =>
          (r, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
            apply(function(r), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }

  /**
   * The contravariant instance for `Function17`.
   */
  implicit def Function17Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#lambda] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R =
        apply =>
          (s, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
            apply(function(s), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }

  /**
   * The contravariant instance for `Function18`.
   */
  implicit def Function18Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#lambda] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S =
        apply =>
          (t, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
            apply(function(t), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    }

  /**
   * The contravariant instance for `Function10`.
   */
  implicit def Function19Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#lambda] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T =
        apply =>
          (u, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
            apply(function(u), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    }

  /**
   * The contravariant instance for `Function20`.
   */
  implicit def Function20Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#lambda] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#lambda
    ] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U =
        apply =>
          (v, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
            apply(function(v), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    }

  /**
   * The contravariant instance for `Function21`.
   */
  implicit def Function21Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]: Contravariant[
    ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#lambda
  ] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#lambda
    ] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V =
        apply =>
          (w, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
            apply(function(w), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    }

  /**
   * The contravariant instance for `Function22`.
   */
  implicit def Function22Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]: Contravariant[
    ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W })#lambda
  ] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W })#lambda
    ] {

      override def contramap[R2, E2, A, R1](function: R1 => R2): (
        (R2, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W =
        apply =>
          (x, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
            apply(function(x), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    }

  /**
   * The contravariant instance for `Schedule`.
   */
  implicit def ScheduleContravariant[R, B]: ContravariantRight[({ type lambda[-x] = Schedule[R, x, B] })#lambda] =
    TriContravariantDivariant.ScheduleTriContravariantDivariant.deriveLeftContravariant

  /**
   * The contravariant instance for `ZIO`.
   */
  implicit def ZIOContravariant[E, A]: Contravariant[({ type lambda[-x] = ZIO[x, E, A] })#lambda] =
    Zivariant.ZioZivariant.deriveContravariant

  /**
   * The contravariant instance for `ZLayer`.
   */
  implicit def ZLayerContravariant[E, ROut]: Contravariant[({ type lambda[-x] = ZLayer[x, E, ROut] })#lambda] =
    Zivariant.ZLayerZivariant.deriveContravariant

  /**
   * The contravariant instance for `ZManaged`.
   */
  implicit def ZManagedContravariant[E, A]: Contravariant[({ type lambda[-x] = ZManaged[x, E, A] })#lambda] =
    Zivariant.ZManagedZivariant.deriveContravariant

  /**
   * The contravariant instance for `ZQueue`.
   */
  implicit def ZQueueContravariant[RA, EA, RB, EB, B]
    : Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] {

      override def contramap[R, E2, A, R1](r: R1 => R): ZQueue[RA, EA, RB, EB, R, B] => ZQueue[RA, EA, RB, EB, R1, B] =
        queue => queue.contramap(r)
    }

  /**
   * The contravariant instance for `ZRef`.
   */
  implicit def ZRefContravariant[EA, EB, B]: Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] {

      override def contramap[R, E2, A, R1](r: R1 => R): ZRef[EA, EB, R, B] => ZRef[EA, EB, R1, B] =
        ref => ref.contramap(r)
    }

  /**
   * The contravariant instance for `ZRefM`.
   */
  implicit def ZRefMContravariant[RA, RB, EA, EB, B]
    : Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] {

      override def contramap[R, E2, A, R1](r: R1 => R): ZRefM[RA, RB, EA, EB, R, B] => ZRefM[RA, RB, EA, EB, R1, B] =
        ref => ref.contramap(r)
    }

  /**
   * The contravariant instance for `ZSink`.
   */
  implicit def ZSinkContravariant[R2, E, L, Z]: Contravariant[({ type lambda[-x] = ZSink[R2, E, x, L, Z] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZSink[R2, E, x, L, Z] })#lambda] {

      override def contramap[R, E2, A, R1](r: R1 => R): ZSink[R2, E, R, L, Z] => ZSink[R2, E, R1, L, Z] =
        sink => sink.contramap(r)
    }

  /**
   * The contravariant instance for `ZStream`.
   */
  implicit def ZStreamContravariant[E, A]: Contravariant[({ type lambda[-x] = ZStream[x, E, A] })#lambda] =
    Zivariant.ZStreamZivariant.deriveContravariant[E, A]
}

trait ContravariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class ContravariantOps[F[-_], A](private val self: F[A]) {
    def contramap[B](f: B => A)(implicit contravariant: Contravariant[F]): F[B] =
      contravariant.contramap(f)(self)
  }
}
