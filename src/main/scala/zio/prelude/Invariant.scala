package zio.prelude

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

import zio.prelude.newtypes.Failure
import zio.{ Cause, Chunk, ChunkBuilder, Exit, Fiber }

trait Invariant[F[_]] {

  def invmap[A, B](f: A <=> B): F[A] <=> F[B]

  def identityLaw1[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    invmap(Equivalence.identity[A]).to(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A <=> B, g: B <=> C)(implicit equal: Equal[F[C]]): Boolean =
    (invmap(f) >>> invmap(g)).to(fa) === invmap(f andThen g).to(fa)

}

object Invariant extends LowPriorityInvariantImplicits /* with InvariantVersionSpecific */ {

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
   * The `Covariant` instance for `Cause`
   */
  implicit def CauseCovariant: Covariant[Cause] =
    new Covariant[Cause] {
      override def map[A, B](f: A => B): Cause[A] => Cause[B] = { cause =>
        cause.map(f)
      }
    }

  /**
   * The `Traversable` instance for `Chunk`.
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
   * The `Covariant` instance for a failed `Either`
   */
  implicit def EitherFailureCovariant[R]: Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      override def map[L, L1](f: L => L1): Failure[Either[L, R]] => Failure[Either[L1, R]] = { either =>
        Failure.wrap {
          Failure.unwrap(either) match {
            case Left(l)  => Left[L1, R](f(l))
            case Right(r) => Right[L1, R](r)
          }
        }
      }
    }

  /**
   * The `Traversable` instance for `Either`.
   */
  implicit def EitherTraversable[E]: Traversable[({ type lambda[+a] = Either[E, a] })#lambda] =
    new Traversable[({ type lambda[+a] = Either[E, a] })#lambda] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](either: Either[E, A])(f: A => G[B]): G[Either[E, B]] =
        either.fold(Left(_).succeed, f(_).map(Right(_)))
    }

  /**
   * The `Covariant` instance for `Exit`
   */
  implicit def ExitCovariant[E]: Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] {
      override def map[A, B](f: A => B): Exit[E, A] => Exit[E, B] = { exit =>
        exit.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `Exit`
   */
  implicit def ExitFailureCovariant[A]: Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] {
      override def map[E, E1](f: E => E1): Failure[Exit[E, A]] => Failure[Exit[E1, A]] = { exit =>
        Failure.wrap(Failure.unwrap(exit).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `Fiber`
   */
  implicit def FiberCovariant[E]: Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def map[A, B](f: A => B): Fiber[E, A] => Fiber[E, B] = { fiber =>
        fiber.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Function1`
   */
  implicit def Function1Covariant[T]: Covariant[({ type lambda[+x] = T => x })#lambda] =
    new Covariant[({ type lambda[+x] = T => x })#lambda] {
      override def map[A, B](f: A => B): (T => A) => T => B =
        function => t => f(function(t))
    }

  /**
   * The `Covariant` instance for `Function2`
   */
  implicit def Function2Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2) => A) => ((T1, T2) => B) =
        function => (t1, t2) => f(function(t1, t2))
    }

  /**
   * The `Covariant` instance for `Function3`
   */
  implicit def Function3Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3) => A) => ((T1, T2, T3) => B) =
        function => (t1, t2, t3) => f(function(t1, t2, t3))
    }

  /**
   * The `Covariant` instance for `Function4`
   */
  implicit def Function4Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4) => A) => ((T1, T2, T3, T4) => B) =
        function => (t1, t2, t3, t4) => f(function(t1, t2, t3, t4))
    }

  /**
   * The `Covariant` instance for `Function5`
   */
  implicit def Function5Covariant[T1, T2, T3, T4, T5]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5) => A) => ((T1, T2, T3, T4, T5) => B) =
        function => (t1, t2, t3, t4, t5) => f(function(t1, t2, t3, t4, t5))
    }

  /**
   * The `Covariant` instance for `Function6`
   */
  implicit def Function6Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6) => A) => ((T1, T2, T3, T4, T5, T6) => B) =
        function => (t1, t2, t3, t4, t5, t6) => f(function(t1, t2, t3, t4, t5, t6))
    }

  /**
   * The `Covariant` instance for `Function7`
   */
  implicit def Function7Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, T7) => A) => ((T1, T2, T3, T4, T5, T6, T7) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7) => f(function(t1, t2, t3, t4, t5, t6, t7))
    }

  /**
   * The `Covariant` instance for `Function8`
   */
  implicit def Function8Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8) => f(function(t1, t2, t3, t4, t5, t6, t7, t8))
    }

  /**
   * The `Covariant` instance for `Function9`
   */
  implicit def Function9Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9))
    }

  /**
   * The `Covariant` instance for `Function10`
   */
  implicit def Function10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
    }

  /**
   * The `Covariant` instance for `Function11`
   */
  implicit def Function11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
    }

  /**
   * The `Covariant` instance for `Function12`
   */
  implicit def Function12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
    }

  /**
   * The `Covariant` instance for `Function13`
   */
  implicit def Function13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
    }

  /**
   * The `Covariant` instance for `Function14`
   */
  implicit def Function14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
    }

  /**
   * The `Covariant` instance for `Function15`
   */
  implicit def Function15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
    }

  /**
   * The `Covariant` instance for `Function16`
   */
  implicit def Function16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
    }

  /**
   * The `Covariant` instance for `Function17`
   */
  implicit def Function17Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
    }

  /**
   * The `Covariant` instance for `Function18`
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
    }

  /**
   * The `Covariant` instance for `Function19`
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
    }

  /**
   * The `Covariant` instance for `Function20`
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
    }

  /**
   * The `Covariant` instance for `Function21`
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
    }

  /**
   * The `Covariant` instance for `Function22`
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => A
      ) => (
        (
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
        ) => B
      ) =
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

  implicit def FutureInvariant(implicit ec: ExecutionContext): Invariant[Future] =
    new Invariant[Future] {
      def invmap[A, B](f: A <=> B): Future[A] <=> Future[B] =
        Equivalence(_.map(f.to), _.map(f.from))
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

  implicit def MapInvariant[V]: Invariant[({ type lambda[x] = Map[x, V] })#lambda] =
    new Invariant[({ type lambda[x] = Map[x, V] })#lambda] {
      def invmap[A, B](f: A <=> B): Map[A, V] <=> Map[B, V] =
        Equivalence(
          mapA => mapA.map { case (k, v) => (f.to(k), v) },
          mapB => mapB.map { case (k, v) => (f.from(k), v) }
        )
    }

  implicit val ListInvariant: Invariant[List] =
    new Invariant[List] {
      def invmap[A, B](f: A <=> B): List[A] <=> List[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val OptionInvariant: Invariant[Option] =
    new Invariant[Option] {
      def invmap[A, B](f: A <=> B): Option[A] <=> Option[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val SetInvariant: Invariant[Set] =
    new Invariant[Set] {
      def invmap[A, B](f: A <=> B): Set[A] <=> Set[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }

  implicit val TryInvariant: Invariant[Try] =
    new Invariant[Try] {
      def invmap[A, B](f: A <=> B): Try[A] <=> Try[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val VectorInvariant: Invariant[Vector] =
    new Invariant[Vector] {
      def invmap[A, B](f: A <=> B): Vector[A] <=> Vector[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }
}

trait LowPriorityInvariantImplicits {

  /**
   * The contravariant instance for `Function1[-A, +B] : [*, *] => *`.
   */
  implicit def Function1Contravariant[B]: Contravariant[({ type lambda[-x] = x => B })#lambda] = {
    type Function1B[-A] = Function1[A, B]

    new Contravariant[Function1B] {
      def contramap[A, C](function: C => A): (A => B) => (C => B) =
        apply => c => apply(function(c))
    }
  }

  /**
   * The contravariant instance for `Function2`.
   */
  implicit def Function2Contravariant[B, C]: Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] {
      def contramap[A, D](function: D => A): ((A, B) => C) => ((D, B) => C) =
        apply => (d, b) => apply(function(d), b)
    }

  /**
   * The contravariant instance for `Function3`.
   */
  implicit def Function3Contravariant[B, C, D]: Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] {
      def contramap[A, E](function: E => A): ((A, B, C) => D) => ((E, B, C) => D) =
        apply => (e, b, c) => apply(function(e), b, c)
    }

  /**
   * The contravariant instance for `Function4`.
   */
  implicit def Function4Contravariant[B, C, D, E]: Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] {
      def contramap[A, F](function: F => A): ((A, B, C, D) => E) => ((F, B, C, D) => E) =
        apply => (f, b, c, d) => apply(function(f), b, c, d)
    }

  /**
   * The contravariant instance for `Function5`.
   */
  implicit def Function5Contravariant[B, C, D, E, F]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] {
      def contramap[A, G](function: G => A): ((A, B, C, D, E) => F) => ((G, B, C, D, E) => F) =
        apply => (g, b, c, d, e) => apply(function(g), b, c, d, e)
    }

  /**
   * The contravariant instance for `Function6`.
   */
  implicit def Function6Contravariant[B, C, D, E, F, G]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] {
      def contramap[A, H](function: H => A): ((A, B, C, D, E, F) => G) => ((H, B, C, D, E, F) => G) =
        apply => (h, b, c, d, e, f) => apply(function(h), b, c, d, e, f)
    }

  /**
   * The contravariant instance for `Function7`.
   */
  implicit def Function7Contravariant[B, C, D, E, F, G, H]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] {
      def contramap[A, I](function: I => A): ((A, B, C, D, E, F, G) => H) => ((I, B, C, D, E, F, G) => H) =
        apply => (i, b, c, d, e, f, g) => apply(function(i), b, c, d, e, f, g)
    }

  /**
   * The contravariant instance for `Function8`.
   */
  implicit def Function8Contravariant[B, C, D, E, F, G, H, I]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] {
      def contramap[A, J](function: J => A): ((A, B, C, D, E, F, G, H) => I) => ((J, B, C, D, E, F, G, H) => I) =
        apply => (j, b, c, d, e, f, g, h) => apply(function(j), b, c, d, e, f, g, h)
    }

  /**
   * The contravariant instance for `Function9`.
   */
  implicit def Function9Contravariant[B, C, D, E, F, G, H, I, J]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] {
      def contramap[A, K](function: K => A): ((A, B, C, D, E, F, G, H, I) => J) => ((K, B, C, D, E, F, G, H, I) => J) =
        apply => (k, b, c, d, e, f, g, h, i) => apply(function(k), b, c, d, e, f, g, h, i)
    }

  /**
   * The contravariant instance for `Function10`.
   */
  implicit def Function10Contravariant[B, C, D, E, F, G, H, I, J, K]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] {
      def contramap[A, L](
        function: L => A
      ): ((A, B, C, D, E, F, G, H, I, J) => K) => ((L, B, C, D, E, F, G, H, I, J) => K) =
        apply => (l, b, c, d, e, f, g, h, i, j) => apply(function(l), b, c, d, e, f, g, h, i, j)
    }

  /**
   * The contravariant instance for `Function11`.
   */
  implicit def Function11Contravariant[B, C, D, E, F, G, H, I, J, K, L]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] {
      def contramap[A, M](
        function: M => A
      ): ((A, B, C, D, E, F, G, H, I, J, K) => L) => ((M, B, C, D, E, F, G, H, I, J, K) => L) =
        apply => (m, b, c, d, e, f, g, h, i, j, k) => apply(function(m), b, c, d, e, f, g, h, i, j, k)
    }

  /**
   * The contravariant instance for `Function12`.
   */
  implicit def Function12Contravariant[B, C, D, E, F, G, H, I, J, K, L, M]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] {
      def contramap[A, N](
        function: N => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L) => M) => ((N, B, C, D, E, F, G, H, I, J, K, L) => M) =
        apply => (n, b, c, d, e, f, g, h, i, j, k, l) => apply(function(n), b, c, d, e, f, g, h, i, j, k, l)
    }

  /**
   * The contravariant instance for `Function13`.
   */
  implicit def Function13Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] {
      def contramap[A, O](
        function: O => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N) => ((O, B, C, D, E, F, G, H, I, J, K, L, M) => N) =
        apply => (o, b, c, d, e, f, g, h, i, j, k, l, m) => apply(function(o), b, c, d, e, f, g, h, i, j, k, l, m)
    }

  /**
   * The contravariant instance for `Function14`.
   */
  implicit def Function14Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] {
      def contramap[A, P](
        function: P => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) => ((P, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) =
        apply => (p, b, c, d, e, f, g, h, i, j, k, l, m, n) => apply(function(p), b, c, d, e, f, g, h, i, j, k, l, m, n)
    }

  /**
   * The contravariant instance for `Function15`.
   */
  implicit def Function15Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] {
      def contramap[A, Q](
        function: Q => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) => ((Q, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) =
        apply =>
          (q, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => apply(function(q), b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }

  /**
   * The contravariant instance for `Function16`.
   */
  implicit def Function16Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] {
      def contramap[A, R](function: R => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q
      ) => ((R, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) =
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
      def contramap[A, S](function: S => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
      ) => ((S, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) =
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
      def contramap[A, T](function: T => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
      ) => ((T, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) =
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
      def contramap[A, U](function: U => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
      ) => ((U, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) =
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
      def contramap[A, V](function: V => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
      ) => ((V, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) =
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
      def contramap[A, W](function: W => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
      ) => ((W, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) =
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
      def contramap[A, X](function: X => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
      ) => ((X, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W) =
        apply =>
          (x, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
            apply(function(x), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    }

}
