package zio.prelude

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

import zio.prelude.newtypes.{ Failure, FailureIn, FailureOut }
import zio.stm.ZSTM
import zio.stream.{ ZSink, ZStream }
import zio.{
  Cause,
  Chunk,
  ChunkBuilder,
  Exit,
  Fiber,
  NonEmptyChunk,
  Schedule,
  ZIO,
  ZLayer,
  ZManaged,
  ZQueue,
  ZRef,
  ZRefM
}

trait Invariant[F[_]] {

  def invmap[A, B](f: A <=> B): F[A] <=> F[B]

  def identityLaw1[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    invmap(Equivalence.identity[A]).to(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A <=> B, g: B <=> C)(implicit equal: Equal[F[C]]): Boolean =
    (invmap(f) >>> invmap(g)).to(fa) === invmap(f andThen g).to(fa)

}

object Invariant extends LowPriorityInvariantImplicits with InvariantVersionSpecific {

  def apply[F[_]](implicit invariant: Invariant[F]): Invariant[F] =
    invariant

  implicit val AssociativeInvariant: Invariant[Associative] =
    new Invariant[Associative] {
      def invmap[A, B](f: A <=> B): Associative[A] <=> Associative[B] =
        Equivalence(
          (a: Associative[A]) => Associative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Associative[B]) => Associative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Cause`
   */
  implicit def CauseCovariant: Covariant[Cause] =
    new Covariant[Cause] {
      override def map[R, E, A, A1](f: A => A1): Cause[A] => Cause[A1] = { cause =>
        cause.map(f)
      }
    }

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) for `Chunk`.
   */
  implicit val ChunkTraversable: Traversable[Chunk] =
    new Traversable[Chunk] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](chunk: Chunk[A])(f: A => G[B]): G[Chunk[B]] =
        chunk.foldLeft(ChunkBuilder.make[B]().succeed)((builder, a) => builder.zipWith(f(a))(_ += _)).map(_.result())
    }

  implicit val CommutativeInvariant: Invariant[Commutative] =
    new Invariant[Commutative] {
      def invmap[A, B](f: A <=> B): Commutative[A] <=> Commutative[B] =
        Equivalence(
          (a: Commutative[A]) => Commutative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Commutative[B]) => Commutative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `Either`
   */
  implicit def EitherFailureCovariant[R]: Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    Bicovariant.EitherBicovariant.deriveFailureCovariant

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) for `Either`.
   */
  implicit def EitherTraversable[E]: Traversable[({ type lambda[+a] = Either[E, a] })#lambda] with Bicovariant[Either] =
    new Traversable[({ type lambda[+a] = Either[E, a] })#lambda] with Bicovariant[Either] {

      def foreach[G[+_]: IdentityBoth: Covariant, A, B](either: Either[E, A])(f: A => G[B]): G[Either[E, B]] =
        either.fold(Left(_).succeed, f(_).map(Right(_)))

      override def bimap[R, E0, A, E1, A1](f: E0 => E1, g: A => A1): Either[E0, A] => Either[E1, A1] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Exit`
   */
  implicit def ExitCovariant[E]: Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] =
    Bicovariant.ExitBicovariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `Exit`
   */
  implicit def ExitFailureCovariant[A]: Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] =
    Bicovariant.ExitBicovariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Fiber`
   */
  implicit def FiberCovariant[E0]: Covariant[({ type lambda[+a] = Fiber[E0, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Fiber[E0, a] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): Fiber[E0, A] => Fiber[E0, A1] = { fiber =>
        fiber.map(f)
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function1`
   */
  implicit def Function1Covariant[T]: Covariant[({ type lambda[+x] = T => x })#lambda] =
    Divariant.Function1Divariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Function2`
   */
  implicit def Function2Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] =
    TriContravariantDivariant.Function2Trivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Function3`
   */
  implicit def Function3Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3) => A) => (T1, T2, T3) => A1 =
        function => (t1, t2, t3) => f(function(t1, t2, t3))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function4`
   */
  implicit def Function4Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4) => A) => (T1, T2, T3, T4) => A1 =
        function => (t1, t2, t3, t4) => f(function(t1, t2, t3, t4))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function5`
   */
  implicit def Function5Covariant[T1, T2, T3, T4, T5]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, T5) => A) => (T1, T2, T3, T4, T5) => A1 =
        function => (t1, t2, t3, t4, t5) => f(function(t1, t2, t3, t4, t5))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function6`
   */
  implicit def Function6Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, T5, T6) => A) => (T1, T2, T3, T4, T5, T6) => A1 =
        function => (t1, t2, t3, t4, t5, t6) => f(function(t1, t2, t3, t4, t5, t6))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function7`
   */
  implicit def Function7Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7) => A) => (T1, T2, T3, T4, T5, T6, T7) => A1 =
        function => (t1, t2, t3, t4, t5, t6, t7) => f(function(t1, t2, t3, t4, t5, t6, t7))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function8`
   */
  implicit def Function8Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8) => A) => (T1, T2, T3, T4, T5, T6, T7, T8) => A1 =
        function => (t1, t2, t3, t4, t5, t6, t7, t8) => f(function(t1, t2, t3, t4, t5, t6, t7, t8))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function9`
   */
  implicit def Function9Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => A) => (T1, T2, T3, T4, T5, T6, T7, T8, T9) => A1 =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function10`
   */
  implicit def Function10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => A) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => A1 =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function11`
   */
  implicit def Function11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => A) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function12`
   */
  implicit def Function12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function13`
   */
  implicit def Function13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function14`
   */
  implicit def Function14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function15`
   */
  implicit def Function15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function16`
   */
  implicit def Function16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function17`
   */
  implicit def Function17Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function18`
   */
  implicit def Function18Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    : Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => x
      })#lambda
    ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => x
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function19`
   */
  implicit def Function19Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    : Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => x
      })#lambda
    ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => x
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function20`
   */
  implicit def Function20Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ]: Covariant[
    ({
      type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => x
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function21`
   */
  implicit def Function21Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => x
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function22`
   */
  implicit def Function22Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => x
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => A1 =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) =>
            f(
              function(
                t1,
                t2,
                t3,
                t4,
                t5,
                t6,
                t7,
                t8,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20,
                t21,
                t22
              )
            )
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `Future`
   */
  implicit def FutureCovariant(implicit ec: ExecutionContext): Covariant[Future] =
    new Covariant[Future] {
      override def map[R, E, A, A1](f: A => A1): Future[A] => Future[A1] = { future =>
        future.map(f)
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `Id`.
   */
  implicit val IdCovariant: Covariant[Id] =
    new Covariant[Id] {
      override def map[R, E, A, A1](f: A => A1): Id[A] => Id[A1] = { id =>
        Id(f(Id.unwrap(id)))
      }
    }

  implicit val IdentityInvariant: Invariant[Identity] =
    new Invariant[Identity] {
      def invmap[A, B](f: A <=> B): Identity[A] <=> Identity[B] =
        Equivalence(
          (a: Identity[A]) => Identity.make[B](f.to(a.identity), (l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Identity[B]) => Identity.make[A](f.from(b.identity), (l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  implicit val InverseInvariant: Invariant[Inverse] =
    new Invariant[Inverse] {
      def invmap[A, B](f: A <=> B): Inverse[A] <=> Inverse[B] =
        Equivalence(
          (a: Inverse[A]) =>
            Inverse.make[B](
              f.to(a.identity),
              (l, r) => f.to(a.combine(f.from(l), f.from(r))),
              (l, r) => f.to(a.inverse(f.from(l), f.from(r)))
            ),
          (b: Inverse[B]) =>
            Inverse.make[A](
              f.from(b.identity),
              (l, r) => f.from(b.combine(f.to(l), f.to(r))),
              (l, r) => f.from(b.inverse(f.to(l), f.to(r)))
            )
        )
    }

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) instance for `List`.
   */
  implicit val ListTraversable: Traversable[List] =
    new Traversable[List] {
      override def foreach[G[+_]: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]] =
        list.foldRight[G[List[B]]](Nil.succeed)((a, bs) => f(a).zipWith(bs)(_ :: _))
    }

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) instance for `Map`.
   */
  implicit def MapTraversable[K]: Traversable[({ type lambda[+v] = Map[K, v] })#lambda] =
    new Traversable[({ type lambda[+v] = Map[K, v] })#lambda] {
      def foreach[G[+_]: IdentityBoth: Covariant, V, V2](map: Map[K, V])(f: V => G[V2]): G[Map[K, V2]] =
        map.foldLeft[G[Map[K, V2]]](Map.empty.succeed) { case (map, (k, v)) =>
          map.zipWith(f(v))((map, v2) => map + (k -> v2))
        }
    }

  /**
   * The `NonEmptyTraversable` (and thus `Traversable`, `Covariant` and `Invariant`) instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkNonEmptyTraversable: NonEmptyTraversable[NonEmptyChunk] =
    new NonEmptyTraversable[NonEmptyChunk] {
      def foreach1[F[+_]: AssociativeBoth: Covariant, A, B](
        nonEmptyChunk: NonEmptyChunk[A]
      )(f: A => F[B]): F[NonEmptyChunk[B]] =
        nonEmptyChunk
          .reduceMapLeft(f(_).map(ChunkBuilder.make() += _))((bs, a) => bs.zipWith(f(a))(_ += _))
          .map(bs => NonEmptyChunk.nonEmpty(bs.result()))
    }

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) instance for `Option`.
   */
  implicit val OptionTraversable: Traversable[Option] =
    new Traversable[Option] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](option: Option[A])(f: A => G[B]): G[Option[B]] =
        option.fold[G[Option[B]]](Option.empty.succeed)(a => f(a).map(Some(_)))
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `Schedule`
   */
  implicit def ScheduleCovariant[R, A]: Covariant[({ type lambda[+b] = Schedule[R, A, b] })#lambda] =
    TriContravariantDivariant.ScheduleTriContravariantDivariant.deriveCovariant

  implicit val SetInvariant: Invariant[Set] =
    new Invariant[Set] {
      def invmap[A, B](f: A <=> B): Set[A] <=> Set[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `Try`
   */
  implicit val TryCovariant: Covariant[Try] =
    new Covariant[Try] {
      override def map[R, E, A, A1](f: A => A1): Try[A] => Try[A1] = { tryA =>
        tryA.map(f)
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple2`
   */
  implicit def Tuple2Covariant[T1]: Covariant[({ type lambda[+x] = (T1, x) })#lambda] =
    Bicovariant.Tuple2Bicovariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple3`
   */
  implicit def Tuple3Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2, x) })#lambda] =
    TriFullCovariant.tuple3TriCovariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple4`
   */
  implicit def Tuple4Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, A)) => (T1, T2, T3, A1) = tuple =>
        (tuple._1, tuple._2, tuple._3, f(tuple._4))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple5`
   */
  implicit def Tuple5Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, A)) => (T1, T2, T3, T4, A1) = tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, f(tuple._5))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple6`
   */
  implicit def Tuple6Covariant[T1, T2, T3, T4, T5]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, T5, A)) => (T1, T2, T3, T4, T5, A1) = tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, f(tuple._6))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple7`
   */
  implicit def Tuple7Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, T5, T6, A)) => (T1, T2, T3, T4, T5, T6, A1) =
        tuple => (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, f(tuple._7))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple8`
   */
  implicit def Tuple8Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ((T1, T2, T3, T4, T5, T6, T7, A)) => (T1, T2, T3, T4, T5, T6, T7, A1) =
        tuple => (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, f(tuple._8))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple9`
   */
  implicit def Tuple9Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, A1) =
        tuple => (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, f(tuple._9))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple10`
   */
  implicit def Tuple10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, A1) =
        tuple =>
          (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, f(tuple._10))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple11`
   */
  implicit def Tuple11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A1) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          f(tuple._11)
        )
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple12`
   */
  implicit def Tuple12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, x) })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A1) =
        tuple =>
          (
            tuple._1,
            tuple._2,
            tuple._3,
            tuple._4,
            tuple._5,
            tuple._6,
            tuple._7,
            tuple._8,
            tuple._9,
            tuple._10,
            tuple._11,
            f(tuple._12)
          )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple13`
   */
  implicit def Tuple13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          f(tuple._13)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple14`
   */
  implicit def Tuple14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          f(tuple._14)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple15`
   */
  implicit def Tuple15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          f(tuple._15)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple16`
   */
  implicit def Tuple16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          f(tuple._16)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple17`
   */
  implicit def Tuple17Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, x) })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, x) })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          f(tuple._17)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple18`
   */
  implicit def Tuple18Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, x) })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, x) })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          f(tuple._18)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple19`
   */
  implicit def Tuple19Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    : Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, x)
      })#lambda
    ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, x)
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          f(tuple._19)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple20`
   */
  implicit def Tuple20Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    : Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, x)
      })#lambda
    ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, x)
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          tuple._19,
          f(tuple._20)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple21`
   */
  implicit def Tuple21Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ]: Covariant[
    ({
      type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, x)
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, x)
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A1) = tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          tuple._19,
          tuple._20,
          f(tuple._21)
        )
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple22`
   */
  implicit def Tuple22Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, x)
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, x)
      })#lambda
    ] {
      override def map[R, E, A, A1](f: A => A1): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A1) =
        tuple =>
          (
            tuple._1,
            tuple._2,
            tuple._3,
            tuple._4,
            tuple._5,
            tuple._6,
            tuple._7,
            tuple._8,
            tuple._9,
            tuple._10,
            tuple._11,
            tuple._12,
            tuple._13,
            tuple._14,
            tuple._15,
            tuple._16,
            tuple._17,
            tuple._18,
            tuple._19,
            tuple._20,
            tuple._21,
            f(tuple._22)
          )
    }

  /**
   * The `Traversable` (and thus `Covariant` and `Invariant`) instance for `Vector`.
   */
  implicit val VectorTraversable: Traversable[Vector] =
    new Traversable[Vector] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](vector: Vector[A])(f: A => G[B]): G[Vector[B]] =
        vector.foldLeft[G[Vector[B]]](Vector.empty.succeed)((bs, a) => bs.zipWith(f(a))(_ :+ _))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `ZIO`
   */
  implicit def ZIOCovariant[R, E]: Covariant[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    Zivariant.ZioZivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZIO`
   */
  implicit def ZIOFailureCovariant[R, A]: Covariant[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    Zivariant.ZioZivariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `ZLayer`
   */
  implicit def ZLayerCovariant[R, E]: Covariant[({ type lambda[+rout] = ZLayer[R, E, rout] })#lambda] =
    Zivariant.ZLayerZivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZLayer`
   */
  implicit def ZLayerFailureCovariant[R, Out]: Covariant[({ type lambda[+e] = Failure[ZLayer[R, e, Out]] })#lambda] =
    Zivariant.ZLayerZivariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `ZManaged`
   */
  implicit def ZManagedCovariant[R, E]: Covariant[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    Zivariant.ZManagedZivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZManaged`
   */
  implicit def ZManagedFailureCovariant[R, A]: Covariant[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    Zivariant.ZManagedZivariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `ZQueue`
   */
  implicit def ZQueueCovariant[RA, RB, EA, EB, A0]
    : Covariant[({ type lambda[+b] = ZQueue[RA, RB, EA, EB, A0, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZQueue[RA, RB, EA, EB, A0, b] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ZQueue[RA, RB, EA, EB, A0, A] => ZQueue[RA, RB, EA, EB, A0, A1] =
        zqueue => zqueue.map(f)
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `ZRef`
   */
  implicit def ZRefCovariant[EA, EB, A0]: Covariant[({ type lambda[+b] = ZRef[EA, EB, A0, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZRef[EA, EB, A0, b] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ZRef[EA, EB, A0, A] => ZRef[EA, EB, A0, A1] = zref => zref.map(f)
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZRef` on its input
   */
  implicit def ZRefFailureInCovariant[EB, A0, B]
    : Covariant[({ type lambda[+ea] = FailureIn[ZRef[ea, EB, A0, B]] })#lambda] =
    new Covariant[({ type lambda[+ea] = FailureIn[ZRef[ea, EB, A0, B]] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): FailureIn[ZRef[A, EB, A0, B]] => FailureIn[ZRef[A1, EB, A0, B]] =
        zref => FailureIn.wrap(FailureIn.unwrap(zref).dimapError(f, identity))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZRef` on its output
   */
  implicit def ZRefFailureOutCovariant[EA, A0, B]
    : Covariant[({ type lambda[+eb] = FailureOut[ZRef[EA, eb, A0, B]] })#lambda] =
    new Covariant[({ type lambda[+eb] = FailureOut[ZRef[EA, eb, A0, B]] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): FailureOut[ZRef[EA, A, A0, B]] => FailureOut[ZRef[EA, A1, A0, B]] =
        zref => FailureOut.wrap(FailureOut.unwrap(zref).dimapError(identity, f))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `ZRefM`
   */
  implicit def ZRefMCovariant[RA, RB, EA, EB, A0]
    : Covariant[({ type lambda[+b] = ZRefM[RA, RB, EA, EB, A0, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZRefM[RA, RB, EA, EB, A0, b] })#lambda] {
      override def map[R, E, A, A1](f: A => A1): ZRefM[RA, RB, EA, EB, A0, A] => ZRefM[RA, RB, EA, EB, A0, A1] = zref =>
        zref.map(f)
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZRefM` on its input
   */
  implicit def ZRefMFailureInACovariant[RA, RB, EB, A0, B]
    : Covariant[({ type lambda[+ea] = FailureIn[ZRefM[RA, RB, ea, EB, A0, B]] })#lambda] =
    new Covariant[({ type lambda[+ea] = FailureIn[ZRefM[RA, RB, ea, EB, A0, B]] })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): FailureIn[ZRefM[RA, RB, A, EB, A0, B]] => FailureIn[ZRefM[RA, RB, A1, EB, A0, B]] = zref =>
        FailureIn.wrap(FailureIn.unwrap(zref).dimapError(f, identity))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZRefM` on its output
   */
  implicit def ZRefMFailureOutCovariant[RA, RB, EA, A0, B]
    : Covariant[({ type lambda[+eb] = FailureOut[ZRefM[RA, RB, EA, eb, A0, B]] })#lambda] =
    new Covariant[({ type lambda[+eb] = FailureOut[ZRefM[RA, RB, EA, eb, A0, B]] })#lambda] {
      override def map[R, E, A, A1](
        f: A => A1
      ): FailureOut[ZRefM[RA, RB, EA, A, A0, B]] => FailureOut[ZRefM[RA, RB, EA, A1, A0, B]] = { zref =>
        FailureOut.wrap(FailureOut.unwrap(zref).dimapError(identity, f))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `ZStream`
   */
  implicit def ZStreamCovariant[R, E]: Covariant[({ type lambda[+o] = ZStream[R, E, o] })#lambda] =
    Zivariant.ZStreamZivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZStream`
   */
  implicit def ZStreamFailureCovariant[R, O]: Covariant[({ type lambda[+e] = Failure[ZStream[R, e, O]] })#lambda] =
    Zivariant.ZStreamZivariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `ZSTM`
   */
  implicit def ZSTMZivariantCovariant[R, E]: Covariant[({ type lambda[+o] = ZSTM[R, E, o] })#lambda] =
    Zivariant.ZSTMZivariant.deriveCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `ZSTM`
   */
  implicit def ZSTMZivariantFailureCovariant[R, O]: Covariant[({ type lambda[+e] = Failure[ZSTM[R, e, O]] })#lambda] =
    Zivariant.ZSTMZivariant.deriveFailureCovariant
}

trait LowPriorityInvariantImplicits {

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function1[-A, +B] : [*, *] => *`.
   */
  implicit def Function1Contravariant[B]: Contravariant[({ type lambda[-x] = x => B })#lambda] =
    Divariant.Function1Divariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function2`.
   */
  implicit def Function2Contravariant[B, C]: Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] =
    TriContravariantDivariant.Function2Trivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function3`.
   */
  implicit def Function3Contravariant[B, C, D]: Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] {
      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C) => D) => (R1, B, C) => D =
        apply => (e, b, c) => apply(function(e), b, c)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function4`.
   */
  implicit def Function4Contravariant[B, C, D, E]: Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] {
      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C, D) => E) => (R1, B, C, D) => E =
        apply => (f, b, c, d) => apply(function(f), b, c, d)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function5`.
   */
  implicit def Function5Contravariant[B, C, D, E, F]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] {
      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C, D, E) => F) => (R1, B, C, D, E) => F =
        apply => (g, b, c, d, e) => apply(function(g), b, c, d, e)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function6`.
   */
  implicit def Function6Contravariant[B, C, D, E, F, G]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] {
      override def contramap[R, E2, A, R1](function: R1 => R): ((R, B, C, D, E, F) => G) => (R1, B, C, D, E, F) => G =
        apply => (h, b, c, d, e, f) => apply(function(h), b, c, d, e, f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function7`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function8`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function9`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function10`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function11`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function12`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function13`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function14`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function15`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function16`.
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
   * The `Contravariant` (and thus `Invariant`) instance for `Function17`.
   */
  implicit def Function17Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R })#lambda] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R =
        apply =>
          (s, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
            apply(function(s), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function18`.
   */
  implicit def Function18Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S })#lambda] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S =
        apply =>
          (t, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
            apply(function(t), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function10`.
   */
  implicit def Function19Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T })#lambda] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T =
        apply =>
          (u, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
            apply(function(u), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function20`.
   */
  implicit def Function20Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#lambda] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U })#lambda
    ] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U =
        apply =>
          (v, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
            apply(function(v), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function21`.
   */
  implicit def Function21Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]: Contravariant[
    ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#lambda
  ] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V })#lambda
    ] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V =
        apply =>
          (w, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
            apply(function(w), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function22`.
   */
  implicit def Function22Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]: Contravariant[
    ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W })#lambda
  ] =
    new Contravariant[
      ({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W })#lambda
    ] {
      override def contramap[R0, E2, A, R1](function: R1 => R0): (
        (R0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
      ) => (R1, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W =
        apply =>
          (x, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
            apply(function(x), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Schedule`.
   */
  implicit def ScheduleContravariant[In, Out]: Contravariant[({ type lambda[-Env] = Schedule[Env, In, Out] })#lambda] =
    TriContravariantDivariant.ScheduleTriContravariantDivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZIO`.
   */
  implicit def ZIOContravariant[E, A]: Contravariant[({ type lambda[-R] = ZIO[R, E, A] })#lambda] =
    Zivariant.ZioZivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZLayer`.
   */
  implicit def ZLayerContravariant[E, ROut]: Contravariant[({ type lambda[-x] = ZLayer[x, E, ROut] })#lambda] =
    Zivariant.ZLayerZivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZManaged`.
   */
  implicit def ZManagedContravariant[E, A]: Contravariant[({ type lambda[-x] = ZManaged[x, E, A] })#lambda] =
    Zivariant.ZManagedZivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZQueue`.
   */
  implicit def ZQueueContravariant[RA, EA, RB, EB, B]
    : Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] {
      override def contramap[R, E2, A, R1](f: R1 => R): ZQueue[RA, EA, RB, EB, R, B] => ZQueue[RA, EA, RB, EB, R1, B] =
        queue => queue.contramap(f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZRef`.
   */
  implicit def ZRefContravariant[EA, EB, B]: Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] {
      override def contramap[R, E2, A, R1](f: R1 => R): ZRef[EA, EB, R, B] => ZRef[EA, EB, R1, B] =
        ref => ref.contramap(f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZRefM`.
   */
  implicit def ZRefMContravariant[RA, RB, EA, EB, B]
    : Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] {

      override def contramap[R, E2, A, R1](f: R1 => R): ZRefM[RA, RB, EA, EB, R, B] => ZRefM[RA, RB, EA, EB, R1, B] =
        ref => ref.contramap(f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZSink`.
   */
  implicit def ZSinkContravariant[R0, E, L, Z]: Contravariant[({ type lambda[-x] = ZSink[R0, E, x, L, Z] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZSink[R0, E, x, L, Z] })#lambda] {

      override def contramap[R, E2, A, R1](f: R1 => R): ZSink[R0, E, R, L, Z] => ZSink[R0, E, R1, L, Z] =
        sink => sink.contramap(f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZStream`.
   */
  implicit def ZStreamContravariant[E, A]: Contravariant[({ type lambda[-x] = ZStream[x, E, A] })#lambda] =
    Zivariant.ZStreamZivariant.deriveContravariant

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZSTM`.
   */
  implicit def ZSTMZivariantContravariant[E, A]: Contravariant[({ type lambda[-x] = ZSTM[x, E, A] })#lambda] =
    Zivariant.ZSTMZivariant.deriveContravariant
}
