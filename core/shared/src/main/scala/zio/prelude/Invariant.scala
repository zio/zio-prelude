/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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

import zio.prelude.coherent.CovariantIdentityBoth
import zio.prelude.newtypes.Failure
import zio.stm.ZSTM
import zio.stream.{ZSink, ZStream}
import zio.{Cause, Chunk, ChunkBuilder, Exit, Fiber, NonEmptyChunk, Schedule, ZIO}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait Invariant[F[_]] { self =>

  def invmap[A, B](f: A <=> B): F[A] <=> F[B]

  def identityLaw1[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    invmap(Equivalence.identity[A]).to(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A <=> B, g: B <=> C)(implicit equal: Equal[F[C]]): Boolean =
    (invmap(f) >>> invmap(g)).to(fa) === invmap(f andThen g).to(fa)

  /**
   * Compose two invariant functors.
   */
  final def compose[G[_]](implicit g: Invariant[G]): Invariant[({ type lambda[A] = F[G[A]] })#lambda] =
    new Invariant[({ type lambda[A] = F[G[A]] })#lambda] {
      def invmap[A, B](f: A <=> B): F[G[A]] <=> F[G[B]] = self.invmap(g.invmap(f))
    }
}

object Invariant extends LowPriorityInvariantImplicits with InvariantVersionSpecific {

  def apply[F[_]](implicit invariant: Invariant[F]): Invariant[F] =
    invariant

  /**
   * The `Invariant` instance for `Associative`
   */
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
      override def map[A, B](f: A => B): Cause[A] => Cause[B] = { cause =>
        cause.map(f)
      }
    }

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) for `Chunk`.
   */
  implicit val ChunkForEach: ForEach[Chunk] =
    new ForEach[Chunk] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](chunk: Chunk[A])(f: A => G[B]): G[Chunk[B]]      =
        CovariantIdentityBoth[G].forEach(chunk)(f)
      override def forEach_[G[+_]: IdentityBoth: Covariant, A](chunk: Chunk[A])(f: A => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(chunk)(f)
    }

  /**
   * The `Invariant` instance for `Commutative`.
   */
  implicit val CommutativeInvariant: Invariant[Commutative] =
    new Invariant[Commutative] {
      def invmap[A, B](f: A <=> B): Commutative[A] <=> Commutative[B] =
        Equivalence(
          (a: Commutative[A]) => Commutative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Commutative[B]) => Commutative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `ForEach` instance for `Const`.
   */
  implicit def ConstForEach[A]: ForEach[({ type ConstA[+B] = Const[A, B] })#ConstA] =
    new ForEach[({ type ConstA[+B] = Const[A, B] })#ConstA] {
      def forEach[G[+_]: IdentityBoth: Covariant, B, C](fa: Const[A, B])(f: B => G[C]): G[Const[A, C]] =
        Const.wrap(Const.unwrap(fa)).succeed
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `Either`
   */
  implicit def EitherFailureCovariant[R]: Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    Bicovariant.EitherBicovariant.deriveFailureCovariant

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) for `Either`.
   */
  implicit def EitherForEach[E]: ForEach[({ type lambda[+a] = Either[E, a] })#lambda] with Bicovariant[Either] =
    new ForEach[({ type lambda[+a] = Either[E, a] })#lambda] with Bicovariant[Either] {

      def forEach[G[+_]: IdentityBoth: Covariant, A, B](either: Either[E, A])(f: A => G[B]): G[Either[E, B]] =
        either.fold(Left(_).succeed, f(_).map(Right(_)))

      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): Either[A, B] => Either[AA, BB] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Exit`
   */
  implicit def ExitCovariant[E]: Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] {
      override def map[A, B](f: A => B): Exit[E, A] => Exit[E, B] = { exit =>
        exit.mapExit(f)
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for a failed `Exit`
   */
  implicit def ExitFailureCovariant[A]: Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] =
    Bicovariant.ExitBicovariant.deriveFailureCovariant

  /**
   * The `Covariant` (and thus `Invariant`) for `Fiber`
   */
  implicit def FiberCovariant[E]: Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def map[A, B](f: A => B): Fiber[E, A] => Fiber[E, B] = { fiber =>
        fiber.map(f)
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function0`
   */
  implicit lazy val Function0Covariant: Covariant[Function0] =
    new Covariant[Function0] {
      override def map[A, B](f: A => B): (() => A) => () => B =
        function => () => f(function())
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function1`
   */
  implicit def Function1Covariant[T]: Covariant[({ type lambda[+x] = T => x })#lambda] =
    Divariant.Function1Divariant.deriveCovariant[T]

  /**
   * The `Covariant` (and thus `Invariant`) for `Function2`
   */
  implicit def Function2Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2) => A) => (T1, T2) => B =
        function => (t1, t2) => f(function(t1, t2))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function3`
   */
  implicit def Function3Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3) => A) => (T1, T2, T3) => B =
        function => (t1, t2, t3) => f(function(t1, t2, t3))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function4`
   */
  implicit def Function4Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4) => A) => (T1, T2, T3, T4) => B =
        function => (t1, t2, t3, t4) => f(function(t1, t2, t3, t4))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function5`
   */
  implicit def Function5Covariant[T1, T2, T3, T4, T5]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5) => A) => (T1, T2, T3, T4, T5) => B =
        function => (t1, t2, t3, t4, t5) => f(function(t1, t2, t3, t4, t5))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function6`
   */
  implicit def Function6Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6) => A) => (T1, T2, T3, T4, T5, T6) => B =
        function => (t1, t2, t3, t4, t5, t6) => f(function(t1, t2, t3, t4, t5, t6))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function7`
   */
  implicit def Function7Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, T7) => A) => (T1, T2, T3, T4, T5, T6, T7) => B =
        function => (t1, t2, t3, t4, t5, t6, t7) => f(function(t1, t2, t3, t4, t5, t6, t7))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function8`
   */
  implicit def Function8Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8) => A) => (T1, T2, T3, T4, T5, T6, T7, T8) => B =
        function => (t1, t2, t3, t4, t5, t6, t7, t8) => f(function(t1, t2, t3, t4, t5, t6, t7, t8))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function9`
   */
  implicit def Function9Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => A) => (T1, T2, T3, T4, T5, T6, T7, T8, T9) => B =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function10`
   */
  implicit def Function10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => A) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => B =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function11`
   */
  implicit def Function11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => B =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Function12`
   */
  implicit def Function12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => A
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => B =
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => A
      ) => (
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
      ) => B =
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
      def map[A, B](f: A => B): Future[A] => Future[B] = { future =>
        future.map(f)
      }
    }

  /**
   * The `NonEmptyForEach` (and thus `ForEach`, `Covariant` and `Invariant`) instance for `Id`.
   */
  implicit val IdNonEmptyForEach: NonEmptyForEach[Id] =
    new NonEmptyForEach[Id] {
      override def forEach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
        f(Id.unwrap(fa)).map(Id(_))

      override def map[A, B](f: A => B): Id[A] => Id[B] = { id =>
        Id(f(Id.unwrap(id)))
      }
    }

  /**
   * The `Invariant` instance for `Identity`
   */
  implicit val IdentityInvariant: Invariant[Identity] =
    new Invariant[Identity] {
      def invmap[A, B](f: A <=> B): Identity[A] <=> Identity[B] =
        Equivalence(
          (a: Identity[A]) => Identity.make[B](f.to(a.identity), (l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Identity[B]) => Identity.make[A](f.from(b.identity), (l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `Invariant` instance for `Inverse`
   */
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
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `List`.
   */
  implicit val ListForEach: ForEach[List] =
    new ForEach[List] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]]     =
        CovariantIdentityBoth[G].forEach(list)(f)
      override def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: List[A])(f: A => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(fa)(f)
      override def map[A, B](f: A => B): List[A] => List[B]                                          =
        _.map(f)
    }

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `Map`.
   */
  implicit def MapForEach[K]: ForEach[({ type lambda[+v] = Map[K, v] })#lambda] =
    new ForEach[({ type lambda[+v] = Map[K, v] })#lambda] {
      def forEach[G[+_]: IdentityBoth: Covariant, V, V2](map: Map[K, V])(f: V => G[V2]): G[Map[K, V2]]  =
        CovariantIdentityBoth[G]
          .forEach[(K, V), (K, V2), Iterable](map) { case (k, v) => f(v).map(k -> _) }
          .map(_.toMap)
      override def forEach_[G[+_]: IdentityBoth: Covariant, V](map: Map[K, V])(f: V => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(map) { case (_, v) => f(v) }
    }

  /**
   * The `NonEmptyForEach` (and thus `ForEach`, `Covariant` and `Invariant`) instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkNonEmptyForEach: NonEmptyForEach[NonEmptyChunk] =
    new NonEmptyForEach[NonEmptyChunk] {
      def forEach1[F[+_]: AssociativeBoth: Covariant, A, B](
        nonEmptyChunk: NonEmptyChunk[A]
      )(f: A => F[B]): F[NonEmptyChunk[B]] =
        nonEmptyChunk
          .reduceMapLeft(f(_).map(ChunkBuilder.make() += _))((bs, a) => bs.zipWith(f(a))(_ += _))
          .map(bs => NonEmptyChunk.nonEmpty(bs.result()))
      override def forEach1_[F[+_]: AssociativeBoth: Covariant, A](nonEmptyChunk: NonEmptyChunk[A])(
        f: A => F[Any]
      ): F[Unit] =
        nonEmptyChunk.reduceMapLeft(f(_))((bs, a) => bs *> f(a)).unit
    }

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `Option`.
   */
  implicit val OptionForEach: ForEach[Option] =
    new ForEach[Option] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](option: Option[A])(f: A => G[B]): G[Option[B]] =
        option.fold[G[Option[B]]](Option.empty.succeed)(a => f(a).map(Some(_)))
    }

  /**
   * The `Covariant` (and thus `Invariant`) instance for `Schedule`
   */
  implicit def ScheduleCovariant[R, A]: Covariant[({ type lambda[+b] = Schedule[R, A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = Schedule[R, A, b] })#lambda] {
      def map[B, B1](f: B => B1): Schedule[R, A, B] => Schedule[R, A, B1] = { schedule =>
        schedule.map(f)
      }
    }

  /** The `Invariant` instance for `Set` */
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
      def map[A, B](f: A => B): Try[A] => Try[B] = { tryA =>
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
    new Covariant[({ type lambda[+x] = (T1, T2, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, A)) => (T1, T2, B) = { tuple =>
        (tuple._1, tuple._2, f(tuple._3))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple4`
   */
  implicit def Tuple4Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, A)) => (T1, T2, T3, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, f(tuple._4))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple5`
   */
  implicit def Tuple5Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, A)) => (T1, T2, T3, T4, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, f(tuple._5))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple6`
   */
  implicit def Tuple6Covariant[T1, T2, T3, T4, T5]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, A)) => (T1, T2, T3, T4, T5, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, f(tuple._6))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple7`
   */
  implicit def Tuple7Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, A)) => (T1, T2, T3, T4, T5, T6, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, f(tuple._7))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple8`
   */
  implicit def Tuple8Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, T7, A)) => (T1, T2, T3, T4, T5, T6, T7, B) = {
        tuple =>
          (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, f(tuple._8))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple9`
   */
  implicit def Tuple9Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, f(tuple._9))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple10`
   */
  implicit def Tuple10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, f(tuple._10))
      }
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple11`
   */
  implicit def Tuple11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, B) = {
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
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple13`
   */
  implicit def Tuple13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, B) = { tuple =>
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
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple14`
   */
  implicit def Tuple14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, B) = { tuple =>
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
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple15`
   */
  implicit def Tuple15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, B) = { tuple =>
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
    }

  /**
   * The `Covariant` (and thus `Invariant`) for `Tuple16`
   */
  implicit def Tuple16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, B) = { tuple =>
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
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, B) = {
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
    }

  /**
   * The `ForEach` (and thus `Covariant` and `Invariant`) instance for `Vector`.
   */
  implicit val VectorForEach: ForEach[Vector] =
    new ForEach[Vector] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](vector: Vector[A])(f: A => G[B]): G[Vector[B]]     =
        CovariantIdentityBoth[G].forEach(vector)(f)
      override def forEach_[G[+_]: IdentityBoth: Covariant, A](vector: Vector[A])(f: A => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(vector)(f)
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
    Divariant.Function1Divariant.deriveContravariant[B]

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function2`.
   */
  implicit def Function2Contravariant[B, C]: Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B) => C })#lambda] {
      def contramap[A, D](function: D => A): ((A, B) => C) => (D, B) => C =
        apply => (d, b) => apply(function(d), b)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function3`.
   */
  implicit def Function3Contravariant[B, C, D]: Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C) => D })#lambda] {
      def contramap[A, E](function: E => A): ((A, B, C) => D) => (E, B, C) => D =
        apply => (e, b, c) => apply(function(e), b, c)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function4`.
   */
  implicit def Function4Contravariant[B, C, D, E]: Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D) => E })#lambda] {
      def contramap[A, F](function: F => A): ((A, B, C, D) => E) => (F, B, C, D) => E =
        apply => (f, b, c, d) => apply(function(f), b, c, d)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function5`.
   */
  implicit def Function5Contravariant[B, C, D, E, F]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E) => F })#lambda] {
      def contramap[A, G](function: G => A): ((A, B, C, D, E) => F) => (G, B, C, D, E) => F =
        apply => (g, b, c, d, e) => apply(function(g), b, c, d, e)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function6`.
   */
  implicit def Function6Contravariant[B, C, D, E, F, G]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F) => G })#lambda] {
      def contramap[A, H](function: H => A): ((A, B, C, D, E, F) => G) => (H, B, C, D, E, F) => G =
        apply => (h, b, c, d, e, f) => apply(function(h), b, c, d, e, f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function7`.
   */
  implicit def Function7Contravariant[B, C, D, E, F, G, H]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G) => H })#lambda] {
      def contramap[A, I](function: I => A): ((A, B, C, D, E, F, G) => H) => (I, B, C, D, E, F, G) => H =
        apply => (i, b, c, d, e, f, g) => apply(function(i), b, c, d, e, f, g)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function8`.
   */
  implicit def Function8Contravariant[B, C, D, E, F, G, H, I]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H) => I })#lambda] {
      def contramap[A, J](function: J => A): ((A, B, C, D, E, F, G, H) => I) => (J, B, C, D, E, F, G, H) => I =
        apply => (j, b, c, d, e, f, g, h) => apply(function(j), b, c, d, e, f, g, h)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function9`.
   */
  implicit def Function9Contravariant[B, C, D, E, F, G, H, I, J]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I) => J })#lambda] {
      def contramap[A, K](function: K => A): ((A, B, C, D, E, F, G, H, I) => J) => (K, B, C, D, E, F, G, H, I) => J =
        apply => (k, b, c, d, e, f, g, h, i) => apply(function(k), b, c, d, e, f, g, h, i)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function10`.
   */
  implicit def Function10Contravariant[B, C, D, E, F, G, H, I, J, K]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J) => K })#lambda] {
      def contramap[A, L](
        function: L => A
      ): ((A, B, C, D, E, F, G, H, I, J) => K) => (L, B, C, D, E, F, G, H, I, J) => K =
        apply => (l, b, c, d, e, f, g, h, i, j) => apply(function(l), b, c, d, e, f, g, h, i, j)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function11`.
   */
  implicit def Function11Contravariant[B, C, D, E, F, G, H, I, J, K, L]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K) => L })#lambda] {
      def contramap[A, M](
        function: M => A
      ): ((A, B, C, D, E, F, G, H, I, J, K) => L) => (M, B, C, D, E, F, G, H, I, J, K) => L =
        apply => (m, b, c, d, e, f, g, h, i, j, k) => apply(function(m), b, c, d, e, f, g, h, i, j, k)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function12`.
   */
  implicit def Function12Contravariant[B, C, D, E, F, G, H, I, J, K, L, M]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L) => M })#lambda] {
      def contramap[A, N](
        function: N => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L) => M) => (N, B, C, D, E, F, G, H, I, J, K, L) => M =
        apply => (n, b, c, d, e, f, g, h, i, j, k, l) => apply(function(n), b, c, d, e, f, g, h, i, j, k, l)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function13`.
   */
  implicit def Function13Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M) => N })#lambda] {
      def contramap[A, O](
        function: O => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N) => (O, B, C, D, E, F, G, H, I, J, K, L, M) => N =
        apply => (o, b, c, d, e, f, g, h, i, j, k, l, m) => apply(function(o), b, c, d, e, f, g, h, i, j, k, l, m)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function14`.
   */
  implicit def Function14Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N) => O })#lambda] {
      def contramap[A, P](
        function: P => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) => (P, B, C, D, E, F, G, H, I, J, K, L, M, N) => O =
        apply => (p, b, c, d, e, f, g, h, i, j, k, l, m, n) => apply(function(p), b, c, d, e, f, g, h, i, j, k, l, m, n)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function15`.
   */
  implicit def Function15Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P })#lambda] {
      def contramap[A, Q](
        function: Q => A
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) => (Q, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P =
        apply =>
          (q, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => apply(function(q), b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Function16`.
   */
  implicit def Function16Contravariant[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
    : Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] =
    new Contravariant[({ type lambda[-x] = (x, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q })#lambda] {
      def contramap[A, R](function: R => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q
      ) => (R, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q =
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
      def contramap[A, S](function: S => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
      ) => (S, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R =
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
      def contramap[A, T](function: T => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
      ) => (T, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S =
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
      def contramap[A, U](function: U => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
      ) => (U, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T =
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
      def contramap[A, V](function: V => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
      ) => (V, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U =
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
      def contramap[A, W](function: W => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
      ) => (W, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V =
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
      def contramap[A, X](function: X => A): (
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
      ) => (X, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W =
        apply =>
          (x, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
            apply(function(x), b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `Schedule`.
   */
  implicit def ScheduleContravariant[R, B]: Contravariant[({ type lambda[-x] = Schedule[R, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = Schedule[R, x, B] })#lambda] {
      def contramap[A, A0](f: A0 => A): Schedule[R, A, B] => Schedule[R, A0, B] =
        schedule => schedule.contramap(f)
    }

  /**
   * The `Contravariant` (and thus `Invariant`) instance for `ZSink`.
   */
  implicit def ZSinkContravariant[R, E, L, Z]: Contravariant[({ type lambda[-x] = ZSink[R, E, x, L, Z] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZSink[R, E, x, L, Z] })#lambda] {
      def contramap[A, C](f: C => A): ZSink[R, E, A, L, Z] => ZSink[R, E, C, L, Z] =
        sink => sink.contramap(f)
    }

}

trait InvariantSyntax {

  /**
   * Provides infix syntax for mapping over invariant values.
   */
  implicit class InvariantOps[F[_], A](private val self: F[A]) {
    def invmap[B](f: A <=> B)(implicit F: Invariant[F]): F[B] =
      F.invmap(f).to(self)
  }
}
