/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

package zio.prelude.laws

import zio.prelude._
import zio.prelude.newtypes.Failure
import zio.test.Gen.oneOf
import zio.test._
import zio.test.laws._
import zio.{Cause, Exit, Has, NonEmptyChunk, Random, ZTraceElement}

import scala.concurrent.Future
import scala.util.Try

/**
 * Provides higher kinded generators.
 */
object GenFs {

  /**
   * A generator of failed `Cause` values.
   */
  val cause: GenF[Has[Random] with Has[Sized], Cause] =
    new GenF[Has[Random] with Has[Sized], Cause] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, Cause[A]] =
        Gen.causes(gen, Gen.throwable)
    }

  def either[R <: Has[Random] with Has[Sized], E](e: Gen[R, E]): GenF[R, ({ type lambda[+a] = Either[E, a] })#lambda] =
    new GenF[R, ({ type lambda[+a] = Either[E, a] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, Either[E, A]] =
        Gen.either(e, a)
    }

  /**
   * A generator of `Exit` values.
   */
  def exit[R <: Has[Random] with Has[Sized], E](
    e: Gen[R, Cause[E]]
  ): GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] =
    new GenF[R, ({ type lambda[+a] = Exit[E, a] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, Exit[E, A]] =
        Gen.either(e, a).map {
          case Left(cause)    => Exit.failCause(cause)
          case Right(success) => Exit.succeed(success)
        }
    }

  /**
   * A generator of `Future` values.
   */
  val future: GenF[Has[Random] with Has[Sized], Future] =
    new GenF[Has[Random] with Has[Sized], Future] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, Future[A]] =
        oneOf(Gen.throwable.map(Future.failed), gen.map(Future.successful))
    }

  def map[R <: Has[Random] with Has[Sized], K](k: Gen[R, K]): GenF[R, ({ type lambda[+v] = Map[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = Map[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V])(implicit trace: ZTraceElement): Gen[R1, Map[K, V]] =
        Gen.mapOf(k, v)
    }

  /**
   * A generator of `NonEmptyChunk` values.
   */
  def nonEmptyChunk: GenF[Has[Random] with Has[Sized], NonEmptyChunk] =
    new GenF[Has[Random] with Has[Sized], NonEmptyChunk] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, NonEmptyChunk[A]] =
        Gen.chunkOf1(gen)
    }

  def nonEmptyList: GenF[Has[Random] with Has[Sized], NonEmptyList] =
    new GenF[Has[Random] with Has[Sized], NonEmptyList] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, NonEmptyList[A]] =
        Gens.nonEmptyListOf(gen)
    }

  def nonEmptySet: GenF[Has[Random] with Has[Sized], NonEmptySet] =
    new GenF[Has[Random] with Has[Sized], NonEmptySet] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, NonEmptySet[A]] =
        Gens.nonEmptySetOf(gen)
    }

  /**
   * A generator of `ParSeq` values.
   */
  def parSeq[R <: Has[Random] with Has[Sized], Z <: Unit](
    z: Gen[R, Z]
  ): GenF[R, ({ type lambda[+x] = ParSeq[Z, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ParSeq[Z, x] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, ParSeq[Z, A]] =
        Gens.parSeq(z, a)
    }

  /**
   * A generator of `Try` values.
   */
  val tryScala: GenF[Has[Random] with Has[Sized], Try] =
    new GenF[Has[Random] with Has[Sized], Try] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, Try[A]] =
        oneOf(Gen.throwable.map(scala.util.Failure(_)), gen.map(scala.util.Success(_)))
    }

  def tuple2[R <: Has[Random] with Has[Sized], A](a: Gen[R, A]): GenF[R, ({ type lambda[+x] = (A, x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = (A, x) })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B])(implicit trace: ZTraceElement): Gen[R1, (A, B)] =
        a.zip(b)
    }

  def tuple3[R <: Has[Random] with Has[Sized], A, B](
    a: Gen[R, A],
    b: Gen[R, B]
  ): GenF[R, ({ type lambda[+c] = (A, B, c) })#lambda] =
    new GenF[R, ({ type lambda[+c] = (A, B, c) })#lambda] {
      def apply[R1 <: R, C](c: Gen[R1, C])(implicit trace: ZTraceElement): Gen[R1, (A, B, C)] =
        a.zip(b).zip(c)
    }

  def validation[R <: Has[Random] with Has[Sized], W, E](
    w: Gen[R, W],
    e: Gen[R, E]
  ): GenF[R, ({ type lambda[+x] = ZValidation[W, E, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = ZValidation[W, E, x] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A])(implicit trace: ZTraceElement): Gen[R1, ZValidation[W, E, A]] =
        Gens.validation(w, e, a)
    }

  def validationFailure[R <: Has[Random] with Has[Sized], W, A](
    w: Gen[R, W],
    a: Gen[R, A]
  ): GenF[R, ({ type lambda[+x] = Failure[ZValidation[W, x, A]] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Failure[ZValidation[W, x, A]] })#lambda] {
      def apply[R1 <: R, E](e: Gen[R1, E])(implicit trace: ZTraceElement): Gen[R1, Failure[ZValidation[W, E, A]]] =
        Gens.validation(w, e, a).map(Failure.wrap)
    }
}
