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

package zio.prelude.fx

import zio._
import zio.prelude._
import zio.prelude.coherent.CovariantIdentityBoth

import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.nowarn
import scala.reflect.ClassTag
import scala.util.Try

/**
 * `ZPure[W, S1, S2, R, E, A]` is a purely functional description of a
 * computation that requires an environment `R` and an initial state `S1` and
 * may either fail with an `E` or succeed with an updated state `S2` and an `A`
 * along with in either case a log with entries of type `W`. Because of its
 * polymorphism `ZPure` can be used to model a variety of effects including
 * context, state, failure, and logging.
 */

sealed abstract class ZPure[+W, -S1, +S2, -R, +E, +A] { self =>
  import ZPure._

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[W1 >: W, S3, R1 <: R, E1 >: E, B](that: ZPure[W1, S2, S3, R1, E1, B]): ZPure[W1, S1, S3, R1, E1, B] =
    self zipRight that

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[W1 >: W, S3, R1 <: R, E1 >: E, B](that: ZPure[W1, S2, S3, R1, E1, B]): ZPure[W1, S1, S3, R1, E1, A] =
    self zipLeft that

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[W1 >: W, S3, R1 <: R, E1 >: E, B](that: ZPure[W1, S2, S3, R1, E1, B])(implicit
    zippable: Zippable[A, B]
  ): ZPure[W1, S1, S3, R1, E1, zippable.Out] =
    self zip that

  /**
   * A symbolic alias for `orElseEither`.
   */
  final def <+>[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, B](
    that: => ZPure[W1, S0, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, Either[A, B]] =
    self orElseEither that

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    that: => ZPure[W1, S0, S3, R1, E1, A1]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, A1] =
    self orElse that

  /**
   * A symbolic alias for `flatMap`.
   */
  final def >>=[W1 >: W, S3, R1 <: R, E1 >: E, B](f: A => ZPure[W1, S2, S3, R1, E1, B]): ZPure[W1, S1, S3, R1, E1, B] =
    self flatMap f

  /**
   * A symbolic alias for `log`.
   */
  final def ??[W1 >: W](w: W1): ZPure[W1, S1, S2, R, E, A] =
    self.log(w)

  /**
   * Submerges the error case of an `Either` into the error type of this
   * computation.
   */
  final def absolve[E1 >: E, B](implicit ev: A <:< Either[E1, B]): ZPure[W, S1, S2, R, E1, B] =
    flatMap(ev(_).fold(fail, succeed))

  /**
   * Maps the success value of this computation to a constant value.
   */
  final def as[B](b: => B): ZPure[W, S1, S2, R, E, B] =
    map(_ => b)

  /**
   * Maps the success value of this computation to the optional value.
   */
  final def asSome: ZPure[W, S1, S2, R, E, Option[A]] =
    map(Some(_))

  /**
   * Maps the error value of this computation to the optional value.
   */
  final def asSomeError(implicit ev: CanFail[E]): ZPure[W, S1, S2, R, Option[E], A] =
    mapError(Some(_))

  /**
   * Maps the output state to a constant value
   */
  final def asState[S3](s: S3): ZPure[W, S1, S3, R, E, A] =
    mapState(_ => s)

  /**
   * Returns a computation whose error and success channels have been mapped
   * by the specified functions, `f` and `g`.
   */
  final def bimap[E1, B](f: E => E1, g: A => B)(implicit ev: CanFail[E]): ZPure[W, S1, S2, R, E1, B] =
    foldM(e => fail(f(e)), a => succeed(g(a)))

  /**
   * Recovers from all errors.
   */
  final def catchAll[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    f: E => ZPure[W1, S0, S3, R1, E1, A1]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, A1] =
    foldM(f, succeed)

  /**
   * Recovers from some or all of the error cases.
   */
  final def catchSome[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[E, ZPure[W1, S0, S3, R1, E1, A1]]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, A1] =
    catchAll(pf.applyOrElse[E, ZPure[W1, S0, S3, R1, E1, A1]](_, fail))

  /**
   * Modifies the behavior of the inner computation regarding logs, so that
   * logs written in a failed computation will be cleared.
   */
  final def clearLogOnError: ZPure[W, S1, S2, R, E, A] =
    ClearLogOnError(value = true, self)

  /**
   * Transforms the result of this computation with the specified partial
   * function, failing with the `e` value if the partial function is not
   * defined for the given input.
   */
  final def collect[E1 >: E, B](e: => E1)(pf: PartialFunction[A, B]): ZPure[W, S1, S2, R, E1, B] =
    collectM(e)(pf.andThen(succeed(_)))

  /**
   * Transforms the result of this computation with the specified partial
   * function which returns a new computation, failing with the `e` value if
   * the partial function is not defined for the given input.
   */
  final def collectM[W1 >: W, S3, R1 <: R, E1 >: E, B](
    e: => E1
  )(pf: PartialFunction[A, ZPure[W1, S2, S3, R1, E1, B]]): ZPure[W1, S1, S3, R1, E1, B] =
    flatMap(pf.applyOrElse[A, ZPure[W1, S2, S3, R1, E1, B]](_, _ => fail(e)))

  /**
   * Transforms the initial state of this computation with the specified
   * function.
   */
  final def contramapState[S0](f: S0 => S1): ZPure[W, S0, S2, R, E, A] =
    update(f) *> self

  /**
   * Returns a computation whose failure and success have been lifted into an
   * `Either`. The resulting computation cannot fail, because the failure case
   * has been exposed as part of the `Either` success case.
   */
  final def either[S3 >: S2 <: S1](implicit ev: CanFail[E]): ZPure[W, S3, S3, R, Nothing, Either[E, A]] =
    fold(Left(_), Right(_))

  /**
   * Applies the specified function if the predicate fails.
   */
  final def filterOrElse[W1 >: W, S3 >: S2, R1 <: R, E1 >: E, A1 >: A](
    p: A => Boolean
  )(f: A => ZPure[W1, S2, S3, R1, E1, A1]): ZPure[W1, S1, S3, R1, E1, A1] =
    self.flatMap {
      case v if !p(v) => f(v)
      case v          => ZPure.succeed(v)
    }

  /**
   * Similar to `filterOrElse`, but instead of a function it accepts the ZPure computation
   * to apply if the predicate fails.
   */
  final def filterOrElse_[W1 >: W, S3 >: S2, R1 <: R, E1 >: E, A1 >: A](p: A => Boolean)(
    zPure: => ZPure[W1, S2, S3, R1, E1, A1]
  ): ZPure[W1, S1, S3, R1, E1, A1] =
    filterOrElse[W1, S3, R1, E1, A1](p)(_ => zPure)

  /**
   * Fails with the specified error if the predicate fails.
   */
  final def filterOrFail[E1 >: E](p: A => Boolean)(e: => E1): ZPure[W, S1, S2, R, E1, A] =
    filterOrElse_[W, S2, R, E1, A](p)(ZPure.fail(e))

  /**
   * Extends this computation with another computation that depends on the
   * result of this computation by running the first computation, using its
   * result to generate a second computation, and running that computation.
   */
  final def flatMap[W1 >: W, S3, R1 <: R, E1 >: E, B](
    f: A => ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, B] =
    FlatMap(self, f)

  /**
   * Flattens a nested computation to a single computation by running the outer
   * computation and then running the inner computation.
   */
  final def flatten[W1 >: W, S3, R1 <: R, E1 >: E, B](implicit
    ev: A <:< ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, B] =
    flatMap(ev)

  /**
   * Swaps the error and success types of this computation.
   */
  final def flip[S3 >: S2 <: S1]: ZPure[W, S3, S3, R, A, E] =
    foldM(succeed, fail)

  /**
   * Folds over the failed or successful results of this computation to yield
   * a computation that does not fail, but succeeds with the value of the left
   * or right function passed to `fold`.
   */
  final def fold[S3 >: S2 <: S1, B](failure: E => B, success: A => B)(implicit
    ev: CanFail[E]
  ): ZPure[W, S3, S3, R, Nothing, B] =
    self.foldM(e => ZPure.succeed(failure(e)), a => ZPure.succeed(success(a)))

  /**
   * Recovers from errors by accepting one computation to execute for the case
   * of an error, and one computation to execute for the case of success.
   */
  final def foldM[W1 >: W, S0 <: S1, S3, R1 <: R, E1, B](
    failure: E => ZPure[W1, S0, S3, R1, E1, B],
    success: A => ZPure[W1, S2, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, B] =
    Fold(self, failure, success)

  /**
   * Exposes the output state into the value channel.
   */
  final def getState: ZPure[W, S1, S2, R, E, (S2, A)] =
    flatMap(a => getWith(s => (s, a)))

  /**
   * Returns a successful computation with the head of the list if the list is
   * non-empty or fails with the error `None` if the list is empty.
   */
  final def head[B](implicit ev: A <:< List[B]): ZPure[W, S1, S2, R, Option[E], B] =
    foldM(
      e => ZPure.fail(Some(e)),
      a =>
        ev(a).headOption match {
          case Some(b) => ZPure.succeed(b)
          case None    => ZPure.fail(None)
        }
    )

  /**
   * Modifies the behavior of the inner computation regarding logs, so that
   * logs written in a failed computation will be kept (this is the default behavior).
   */
  final def keepLogOnError: ZPure[W, S1, S2, R, E, A] =
    ClearLogOnError(value = false, self)

  /**
   * Returns a successful computation if the value is `Left`, or fails with error `None`.
   */
  final def left[B, C](implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, Option[E], B] =
    foldM(
      e => ZPure.fail(Some(e)),
      a => ev(a).fold(ZPure.succeed, _ => ZPure.fail(None))
    )

  /**
   * Returns a successful computation if the value is `Left`, or fails with error `e`.
   */
  final def leftOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, E1, B] =
    flatMap(ev(_) match {
      case Right(_)    => ZPure.fail(e)
      case Left(value) => ZPure.succeed(value)
    })

  /**
   * Returns a successful computation if the value is `Left`, or fails with the given error function `e`.
   */
  final def leftOrFailWith[B, C, E1 >: E](e: C => E1)(implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, E1, B] =
    flatMap(ev(_) match {
      case Right(err)  => ZPure.fail(e(err))
      case Left(value) => ZPure.succeed(value)
    })

  /**
   * Returns a successful computation if the value is `Left`, or fails with a [[java.util.NoSuchElementException]].
   */
  final def leftOrFailWithException[B, C, E1 >: NoSuchElementException](implicit
    ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZPure[W, S1, S2, R, E1, B] =
    foldM(
      e => ZPure.fail(ev2(e)),
      a => ev(a).fold(ZPure.succeed, _ => ZPure.fail(new NoSuchElementException("Either.left.get on Right")))
    )

  final def log[W1 >: W](w: W1): ZPure[W1, S1, S2, R, E, A] =
    self.flatMap(a => ZPure.log(w).as(a))

  /**
   * Transforms the result of this computation with the specified function.
   */
  final def map[B](f: A => B): ZPure[W, S1, S2, R, E, B] =
    FMap(self, f)

  /**
   * Transforms the error type of this computation with the specified
   * function.
   */
  final def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): ZPure[W, S1, S2, R, E1, A] =
    catchAll(e => fail(f(e)))

  /**
   * Transforms the updated state of this computation with the specified
   * function.
   */
  final def mapState[S3](f: S2 => S3): ZPure[W, S1, S3, R, E, A] =
    self <* update(f)

  /**
   * Negates the boolean value of this computation.
   */
  final def negate(implicit ev: A <:< Boolean): ZPure[W, S1, S2, R, E, Boolean] =
    map(!_)

  /**
   * Requires the value of this computation to be `None`, otherwise fails with `None`.
   */
  final def none[B](implicit ev: A <:< Option[B]): ZPure[W, S1, S2, R, Option[E], Unit] =
    self.foldM(
      e => ZPure.fail(Some(e)),
      a => a.fold[ZPure[W, S2, S2, R, Option[E], Unit]](ZPure.unit)(_ => ZPure.fail(None))
    )

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise executes the specified computation.
   */
  final def orElse[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    that: => ZPure[W1, S0, S3, R1, E1, A1]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, A1] =
    foldM(_ => that, succeed)

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise executes the specified computation.
   */
  final def orElseEither[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, B](
    that: => ZPure[W1, S0, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, Either[A, B]] =
    foldM(_ => that.map(Right(_)), a => succeed(Left(a)))

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise fails with the specified error.
   */
  final def orElseFail[E1](e1: => E1)(implicit ev: CanFail[E]): ZPure[W, S1, S2, R, E1, A] =
    orElse(fail(e1))

  /**
   * Returns an computation that will produce the value of this computation, unless it
   * fails with the `None` value, in which case it will produce the value of
   * the specified computation.
   */
  final def orElseOptional[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    that: => ZPure[W1, S0, S3, R1, Option[E1], A1]
  )(implicit ev: E <:< Option[E1]): ZPure[W1, S0, S3, R1, Option[E1], A1] =
    catchAll(ev(_).fold(that)(e => ZPure.fail(Some(e))))

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise succeeds with the specified value.
   */
  final def orElseSucceed[A1 >: A](a1: => A1)(implicit ev: CanFail[E]): ZPure[W, S1, Any, R, Nothing, A1] =
    orElse(succeed(a1))

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise fallbacks to the new state with the specified value.
   */
  final def orElseFallback[A1 >: A, S3 >: S2](a1: => A1, s3: => S3)(implicit
    ev: CanFail[E]
  ): ZPure[W, S1, S3, R, Nothing, A1] =
    orElse(set(s3).map(_ => a1))

  /**
   * Provides this computation with its required environment.
   */
  final def provideEnvironment(r: ZEnvironment[R]): ZPure[W, S1, S2, Any, E, A] =
    ZPure.Provide(r, self)

  /**
   * Provides this computation with the single service it requires. If the
   * computation requires multiple services use `provideEnvironment` instead.
   */
  final def provideService[Service <: R](service: => Service)(implicit tag: Tag[Service]): ZPure[W, S1, S2, Any, E, A] =
    provideEnvironment(ZEnvironment(service))

  /**
   * Provides this computation with part of its required environment, leaving
   * the remainder.
   */
  final def provideSomeEnvironment[R0](f: ZEnvironment[R0] => ZEnvironment[R]): ZPure[W, S1, S2, R0, E, A] =
    ZPure.environmentWithPure(r0 => self.provideEnvironment(f(r0)))

  /**
   * Provides this computation with its initial state.
   */
  final def provideState(s: S1): ZPure[W, Any, S2, R, E, A] =
    set(s) *> self

  /**
   * Keeps some of the errors, and `throw` the rest
   */
  final def refineOrDie[E1](
    pf: PartialFunction[E, E1]
  )(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZPure[W, S1, S2, R, E1, A] =
    refineOrDieWith(pf)(ev1)

  /**
   * Keeps some of the errors, and `throw` the rest, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  final def refineOrDieWith[E1](pf: PartialFunction[E, E1])(f: E => Throwable)(implicit
    ev: CanFail[E]
  ): ZPure[W, S1, S2, R, E1, A] =
    self catchAll (err => (pf lift err).fold[ZPure[W, S1, S2, R, E1, A]](throw f(err))(ZPure.fail))

  /**
   * Fail with the returned value if the `PartialFunction` matches, otherwise
   * continue with our held value.
   */
  final def reject[S0 <: S1, S3 >: S2, R1 <: R, E1 >: E](pf: PartialFunction[A, E1]): ZPure[W, S0, S3, R1, E1, A] =
    rejectM(pf.andThen(ZPure.fail(_)))

  /**
   * Continue with the returned computation if the `PartialFunction` matches,
   * translating the successful match into a failure, otherwise continue with
   * our held value.
   */
  final def rejectM[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1 >: E](
    pf: PartialFunction[A, ZPure[W1, S2, S3, R1, E1, E1]]
  ): ZPure[W1, S0, S3, R1, E1, A] =
    self.flatMap[W1, S3, R1, E1, A] { v =>
      if (pf.isDefinedAt(v)) {
        pf(v).flatMap[W1, S3, R1, E1, A](ZPure.fail)
      } else {
        ZPure.succeed(v)
      }
    }

  /**
   * Repeats this computation the specified number of times (or until the first failure)
   * passing the updated state to each successive repetition.
   */
  final def repeatN(n: Int)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A] =
    self.flatMap(a => if (n <= 0) ZPure.succeed(a) else repeatN(n - 1).contramapState(ev))

  /**
   * Repeats this computation until its value satisfies the specified predicate
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatUntil(f: A => Boolean)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A] =
    self.flatMap(a => if (f(a)) ZPure.succeed(a) else repeatUntil(f).contramapState(ev))

  /**
   * Repeats this computation until its value is equal to the specified value
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatUntilEquals[A1 >: A](a: => A1)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A1] =
    repeatUntil(_ == a)

  /**
   * Repeats this computation until the updated state satisfies the specified predicate
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatUntilState(f: S2 => Boolean)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A] =
    self.zip(ZPure.getWith(f)).flatMap { case (a, b) =>
      if (b) ZPure.succeed(a)
      else repeatUntilState(f).contramapState(ev)
    }

  /**
   * Repeats this computation until the updated state is equal to the specified value
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatUntilStateEquals[S3 >: S2](s: => S3)(implicit ev: S2 <:< S1): ZPure[W, S1, S3, R, E, A] =
    repeatUntilState(_ == s)

  /**
   * Repeats this computation for as long as its value satisfies the specified predicate
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatWhile(f: A => Boolean)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A] =
    repeatUntil(!f(_))

  /**
   * Repeats this computation for as long as its value is equal to the specified value
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatWhileEquals[A1 >: A](a: => A1)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A1] =
    repeatWhile(_ == a)

  /**
   * Repeats this computation for as long as the updated state satisfies the specified predicate
   * (or until the first failure) passing the updated state to each successive repetition.
   */
  final def repeatWhileState(f: S2 => Boolean)(implicit ev: S2 <:< S1): ZPure[W, S1, S2, R, E, A] =
    repeatUntilState(!f(_))

  /**
   * Runs this computation to produce its result.
   */
  final def run(implicit ev1: Unit <:< S1, ev2: Any <:< R, ev3: E <:< Nothing): A =
    runResult(())

  /**
   * Runs this computation with the specified initial state, returning both
   * the updated state and the result.
   */
  final def run(s: S1)(implicit ev1: Any <:< R, ev2: E <:< Nothing): (S2, A) =
    runAll(s)._2.fold(ev2, identity)

  /**
   * Runs this computation with the specified initial state, returning both the
   * log and either all the failures that occurred or the updated state and the
   * result.
   */
  final def runAll(s: S1)(implicit ev: Any <:< R): (Chunk[W], Either[E, (S2, A)]) =
    Runner(s, self)

  /**
   * Runs this computation to produce its result or the first failure to
   * occur.
   */
  final def runEither(implicit ev1: Unit <:< S1, ev2: Any <:< R): Either[E, A] =
    runAll(())._2.fold(error => Left(error), { case (_, a) => Right(a) })

  /**
   * Runs this computation to produce its result and the log.
   */
  final def runLog(implicit ev1: Unit <:< S1, ev2: Any <:< R, ev3: E <:< Nothing): (Chunk[W], A) = {
    val (log, either) = runAll(())
    (log, either.fold(error => ev3(error), { case (_, a) => a }))
  }

  /**
   * Runs this computation with the specified initial state, returning the
   * result and discarding the updated state.
   */
  final def runResult(s: S1)(implicit ev1: Any <:< R, ev2: E <:< Nothing): A =
    run(s)._2

  /**
   * Runs this computation with the specified initial state, returning the
   * updated state and discarding the result.
   */
  final def runState(s: S1)(implicit ev1: Any <:< R, ev2: E <:< Nothing): S2 =
    run(s)._1

  /**
   * Runs this computation to a `ZValidation` value.
   */
  final def runValidation(implicit ev1: Unit <:< S1, ev2: Any <:< R): ZValidation[W, E, A] =
    runAll(()) match {
      case (log, Left(error))   => ZValidation.Failure(log, NonEmptyChunk.single(error))
      case (log, Right((_, a))) => ZValidation.Success(log, a)
    }

  /**
   * Converts an option on values into an option on errors leaving the state unchanged.
   */
  final def some[B](implicit ev: A <:< Option[B]): ZPure[W, S1, S2, R, Option[E], B] =
    self.foldM(
      e => ZPure.fail(Some(e)),
      a => a.fold[ZPure[W, S2, S2, R, Option[E], B]](ZPure.fail(Option.empty))(ZPure.succeed)
    )

  /**
   * Extracts the optional value or returns the given 'default' leaving the state unchanged.
   */
  final def someOrElse[B](default: => B)(implicit ev: A <:< Option[B]): ZPure[W, S1, S2, R, E, B] =
    map(_.getOrElse(default))

  /**
   * Extracts the optional value or runs the specified computation passing the
   * updated state from this computation.
   */
  final def someOrElseM[W1 >: W, S3 >: S2, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  )(implicit ev: A <:< Option[B]): ZPure[W1, S1, S3, R1, E1, B] =
    self.flatMap(ev(_).fold(that)(ZPure.succeed))

  /**
   * Extracts the optional value or fails with the given error 'e'.
   */
  final def someOrFail[B, E1 >: E](e: => E1)(implicit ev: A <:< Option[B]): ZPure[W, S1, S2, R, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZPure.succeed(value)
      case None        => ZPure.fail(e)
    })

  /**
   * Extracts the optional value or fails with a [[java.util.NoSuchElementException]] leaving the state unchanged.
   */
  final def someOrFailException[B, E1 >: E](implicit
    ev: A <:< Option[B],
    ev2: NoSuchElementException <:< E1
  ): ZPure[W, S1, S2, R, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZPure.succeed(value)
      case None        => ZPure.fail(new NoSuchElementException("None.get"))
    })

  /**
   * Transforms ZPure to ZIO that either succeeds with `A` or fails with error(s) `E`.
   * The original state is supposed to be `()`.
   */
  final def toZIO(implicit ev: Unit <:< S1): zio.ZIO[R, E, A] =
    ZIO.environmentWithZIO[R] { r =>
      provideEnvironment(r).runAll(())._2 match {
        case Left(error)   => ZIO.fail(error)
        case Right((_, a)) => ZIO.succeed(a)
      }
    }

  /**
   * Transforms ZPure to ZIO that either succeeds with `A` or fails with error(s) `E`.
   */
  final def toZIOWith(s1: S1): zio.ZIO[R, E, A] =
    ZIO.environmentWithZIO[R] { r =>
      val result = provideEnvironment(r).runAll(s1)
      result._2 match {
        case Left(error)   => ZIO.fail(error)
        case Right((_, a)) => ZIO.succeed(a)
      }
    }

  /**
   * Transforms ZPure to ZIO that either succeeds with `S2` and `A` or fails with error(s) `E`.
   */
  final def toZIOWithState(s1: S1): zio.ZIO[R, E, (S2, A)] =
    ZIO.environmentWithZIO[R] { r =>
      val result = provideEnvironment(r).runAll(s1)
      result._2 match {
        case Left(error)   => ZIO.fail(error)
        case Right(result) => ZIO.succeed(result)
      }
    }

  /**
   * Transforms ZPure to ZIO that either succeeds with `Chunk[W]`, `S2` and `A` or fails with error(s) `E`.
   */
  final def toZIOWithAll(s1: S1): ZIO[R, E, (Chunk[W], S2, A)] =
    ZIO.environmentWithZIO[R] { r =>
      val (log, result) = provideEnvironment(r).runAll(s1)
      result match {
        case Left(error)    => ZIO.fail(error)
        case Right((s2, a)) => ZIO.succeed((log, s2, a))
      }
    }

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both into a tuple.
   */
  final def zip[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  )(implicit zippable: Zippable[A, B]): ZPure[W1, S1, S3, R1, E1, zippable.Out] =
    self.zipWith(that)(zippable.zip(_, _))

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and returning the
   * result of this computation.
   */
  final def zipLeft[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, A] =
    self.zipWith(that)((a, _) => a)

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and returning the
   * result of that computation.
   */
  final def zipRight[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, B] =
    self.flatMap(_ => that)

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both using the specified function.
   */
  final def zipWith[W1 >: W, S3, R1 <: R, E1 >: E, B, C](
    that: ZPure[W1, S2, S3, R1, E1, B]
  )(f: (A, B) => C): ZPure[W1, S1, S3, R1, E1, C] =
    self.flatMap(a => that.map(b => f(a, b)))

  /**
   * Returns a successful computation if the value is `Right`, or fails with error `None`.
   */
  final def right[B, C](implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, Option[E], C] =
    self.foldM(
      e => ZPure.fail(Some(e)),
      a => ev(a).fold(_ => ZPure.fail(None), ZPure.succeed)
    )

  /*
     Returns a successful computation if the value is `Right`, or fails with error `e`.
   */
  final def rightOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, E1, C] =
    self.flatMap(ev(_) match {
      case Right(value) => ZPure.succeed(value)
      case Left(_)      => ZPure.fail(e)
    })

  /*
     Returns a successful computation if the value is `Right`, or fails with error function `e`.
   */
  final def rightOrFailWith[B, C, E1 >: E](e: B => E1)(implicit ev: A <:< Either[B, C]): ZPure[W, S1, S2, R, E1, C] =
    self.flatMap(ev(_) match {
      case Right(value) => ZPure.succeed(value)
      case Left(err)    => ZPure.fail(e(err))
    })

  /*
     Returns a successful computation if the value is `Right`, or fails with a [[java.util.NoSuchElementException]].
   */
  final def rightOrFailWithException[B, C, E1 >: NoSuchElementException](implicit
    ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZPure[W, S1, S2, R, E1, C] =
    self.foldM(
      e => ZPure.fail(ev2(e)),
      a => ev(a).fold(_ => ZPure.fail(new NoSuchElementException("Either.right.get on Left")), ZPure.succeed)
    )

  /**
   * Maps the value of this computation to unit.
   */
  final def unit: ZPure[W, S1, S2, R, E, Unit] =
    self.flatMap(succeedUnitFn)

}

object ZPure {
  private val succeedUnit: ZPure[Nothing, Any, Nothing, Any, Nothing, Unit]            = Succeed(())
  private val succeedNone: ZPure[Nothing, Any, Nothing, Any, Nothing, Option[Nothing]] = Succeed(None)
  private val succeedUnitFn                                                            = (_: Any) => succeedUnit

  /**
   * Constructs a computation, catching any `Throwable` that is thrown.
   */
  def attempt[S, A](a: => A): ZPure[Nothing, S, S, Any, Throwable, A] =
    suspend {
      try ZPure.succeed(a)
      catch {
        case e: VirtualMachineError => throw e
        case e: Throwable           => ZPure.fail(e)
      }
    }

  /**
   * Evaluate each computation in the structure from left to right, collecting
   * the successful values and discarding the empty cases.
   */
  def collect[W, S, R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZPure[W, S, S, R, E, Option[B]]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
    if (in.isEmpty) ZPure.succeed(bf.fromSpecific(in)(Nil))
    else
      ZPure.suspend {
        val iterator = in.iterator
        val builder  = bf.newBuilder(in)

        lazy val recurse: Option[B] => ZPure[W, S, S, R, E, Collection[B]] = {
          case Some(b) => builder += b; loop()
          case None    => loop()
        }

        def loop(): ZPure[W, S, S, R, E, Collection[B]] =
          if (iterator.hasNext) f(iterator.next()).flatMap(recurse)
          else ZPure.succeed(builder.result())

        loop()
      }

  /**
   * Combines a collection of computations into a single computation that
   * passes the updated state from each computation to the next and collects
   * the results.
   */
  def collectAll[F[+_]: ForEach, W, S, R, E, A](fa: F[ZPure[W, S, S, R, E, A]]): ZPure[W, S, S, R, E, F[A]] =
    ForEach[F].flip[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda, A](fa)

  /**
   * Accesses the whole environment of the computation.
   */
  def environment[S, R]: ZPure[Nothing, S, S, R, Nothing, ZEnvironment[R]] =
    environmentWith(identity)

  /**
   * Accesses the environment of the computation.
   */
  def environmentWith[R]: EnvironmentWithPartiallyApplied[R] =
    new EnvironmentWithPartiallyApplied

  /**
   * Accesses the environment of the computation in the context of a
   * computation.
   */
  def environmentWithPure[R]: EnvironmentWithPurePartiallyApplied[R] =
    new EnvironmentWithPurePartiallyApplied

  def fail[E](e: E): ZPure[Nothing, Any, Nothing, Any, E, Nothing] =
    ZPure.Fail(e)

  /**
   * Constructs a computation from an `Either`.
   */
  def fromEither[S, L, R](either: Either[L, R]): ZPure[Nothing, S, S, Any, L, R] =
    either.fold(l => ZPure.fail(l), r => ZPure.succeed(r))

  /**
   * Constructs a computation from an `Option`.
   */
  def fromOption[S, A](option: Option[A]): ZPure[Nothing, S, S, Any, Unit, A] =
    option match {
      case Some(a) => ZPure.succeed(a)
      case None    => ZPure.fail(())
    }

  /**
   * Constructs a computation from a `scala.util.Try`.
   */
  def fromTry[S, A](t: Try[A]): ZPure[Nothing, S, S, Any, Throwable, A] =
    attempt(t).flatMap {
      case scala.util.Success(v) => ZPure.succeed(v)
      case scala.util.Failure(t) => ZPure.fail(t)
    }

  /**
   * Maps each element of a collection to a computation and combines them all
   * into a single computation that passes the updated state from each
   * computation to the next and collects the results.
   */
  def forEach[F[+_]: ForEach, W, S, R, E, A, B](fa: F[A])(
    f: A => ZPure[W, S, S, R, E, B]
  ): ZPure[W, S, S, R, E, F[B]] =
    ForEach[F].forEach[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda, A, B](fa)(f)

  /**
   * Maps each element of a collection to a computation and combines them all
   * into a single computation that passes the updated state from each
   * computation to the next and collects the results.
   */
  def foreach[W, S, R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZPure[W, S, S, R, E, B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
    if (in.isEmpty) ZPure.succeed(bf.fromSpecific(in)(Nil))
    else
      ZPure.suspend {
        val iterator = in.iterator
        val builder  = bf.newBuilder(in)

        lazy val recurse: B => ZPure[W, S, S, R, E, Collection[B]] = { b =>
          builder += b
          loop()
        }

        def loop(): ZPure[W, S, S, R, E, Collection[B]] =
          if (iterator.hasNext) f(iterator.next()).flatMap(recurse)
          else ZPure.succeed(builder.result())

        loop()
      }

  /**
   * Maps each element of a collection to a computation and combines them all
   * into a single computation that passes the updated state from each
   * computation to the next and discards the results.
   */
  def foreachDiscard[W, S, R, E, A, B](in: Iterable[A])(f: A => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, Unit] =
    if (in.isEmpty) unit
    else
      ZPure.suspend {
        val iterator = in.iterator

        lazy val recurse: Any => ZPure[W, S, S, R, E, Unit] = _ => loop()

        def loop(): ZPure[W, S, S, R, E, Unit] =
          if (iterator.hasNext) f(iterator.next()).flatMap(recurse)
          else ZPure.unit

        loop()
      }

  /**
   * Constructs a computation that returns the initial state unchanged.
   */
  def get[S]: ZPure[Nothing, S, S, Any, Nothing, S] =
    getWith(identity)

  /**
   * Constructs a computation that applies the provided function to the initial state and returns the result
   */
  def getWith[S, A](f: S => A): ZPure[Nothing, S, S, Any, Nothing, A] =
    Inspect(f)

  def log[S, W](w: W): ZPure[W, S, S, Any, Nothing, Unit] =
    ZPure.Log(w)

  /**
   * Constructs a computation from the specified modify function.
   */
  def modify[S1, S2, A](f: S1 => (A, S2)): ZPure[Nothing, S1, S2, Any, Nothing, A] =
    getWith(f).flatMap { case (a, s2) => set(s2).map(_ => a) }

  /**
   * Constructs a computation that may fail from the specified modify function.
   */
  def modifyEither[S1, S2, E, A](f: S1 => Either[E, (A, S2)]): ZPure[Nothing, S1, S2, Any, E, A] =
    getWith(f).flatMap {
      case Right((a, s2)) => set(s2).map(_ => a)
      case Left(e)        => ZPure.fail(e)
    }

  /**
   * Constructs a computation that succeeds with the `None` value.
   */
  def none[S]: ZPure[Nothing, S, S, Any, Nothing, Option[Nothing]] =
    succeedNone

  /**
   * Accesses the specified service in the environment of the computation.
   */
  def service[S, R: Tag]: ZPure[Nothing, S, S, R, Nothing, R] =
    serviceWith(identity)

  /**
   * Accesses the specified service in the environment of the computation.
   */
  def serviceWith[R]: ServiceWithPartiallyApplied[R] =
    new ServiceWithPartiallyApplied

  /**
   * Accesses the specified service in the environment of the computation in
   * the context of a computation.
   */
  def serviceWithPure[R]: ServiceWithPurePartiallyApplied[R] =
    new ServiceWithPurePartiallyApplied

  /**
   * Constructs a computation that sets the state to the specified value.
   */
  def set[S](s: S): ZPure[Nothing, Any, S, Any, Nothing, Unit] =
    update(_ => s)

  /**
   * Constructs a computation that always succeeds with the specified value,
   * passing the state through unchanged.
   */
  def succeed[S, A](a: A): ZPure[Nothing, S, S, Any, Nothing, A] =
    Succeed(a)

  /**
   * Returns a lazily constructed computation, whose construction may itself require effects.
   */
  def suspend[W, S1, S2, R, E, A](pure: => ZPure[W, S1, S2, R, E, A]): ZPure[W, S1, S2, R, E, A] =
    ZPure.unit.flatMap(_ => pure)

  /**
   * Constructs a computation that always returns the `Unit` value, passing the
   * state through unchanged.
   */
  def unit[S]: ZPure[Nothing, S, S, Any, Nothing, Unit] =
    succeedUnit

  /**
   * The moral equivalent of `if (!p) exp`
   */
  def unless[W, S, R, E, A](p: Boolean)(pure: => ZPure[W, S, S, R, E, A]): ZPure[W, S, S, R, E, Option[A]] =
    if (p) none else pure.asSome

  /**
   * Constructs a computation from the specified update function.
   */
  def update[S1, S2](f: S1 => S2): ZPure[Nothing, S1, S2, Any, Nothing, Unit] =
    Update(f)

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when[W, S, R, E, A](p: Boolean)(pure: => ZPure[W, S, S, R, E, A]): ZPure[W, S, S, R, E, Option[A]] =
    if (p) pure.asSome else none

  /**
   * Runs a computation when the supplied `PartialFunction` matches for the given
   * value, otherwise does nothing.
   */
  def whenCase[W, S, R, E, A, B](a: A)(
    pf: PartialFunction[A, ZPure[W, S, S, R, E, B]]
  ): ZPure[W, S, S, R, E, Option[B]] =
    pf.andThen(_.asSome).applyOrElse(a, (_: A) => none)

  final class EnvironmentWithPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[S, A](f: ZEnvironment[R] => A): ZPure[Nothing, S, S, R, Nothing, A] =
      Environment(f)
  }

  final class EnvironmentWithPurePartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[W, S1, S2, E, A](f: ZEnvironment[R] => ZPure[W, S1, S2, Any, E, A]): ZPure[W, S1, S2, R, E, A] =
      Environment(f).flatten
  }

  final class ServiceWithPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[S, A](f: R => A)(implicit tag: Tag[R]): ZPure[Nothing, S, S, R, Nothing, A] =
      environmentWith(env => f(env.get))
  }

  final class ServiceWithPurePartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[W, S1, S2, E, A](
      f: R => ZPure[W, S1, S2, Any, E, A]
    )(implicit tag: Tag[R]): ZPure[W, S1, S2, R, E, A] =
      environmentWithPure(env => f(env.get))
  }

  /**
   * The `Covariant` instance for `ZPure`.
   */
  implicit def ZPureCovariant[W, S1, S2, R, E]: Covariant[({ type lambda[+A] = ZPure[W, S1, S2, R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = ZPure[W, S1, S2, R, E, A] })#lambda] {
      def map[A, B](f: A => B): ZPure[W, S1, S2, R, E, A] => ZPure[W, S1, S2, R, E, B] =
        _.map(f)
    }

  /**
   * The `IdentityBoth` instance for `ZPure`.
   */
  implicit def ZPureCovariantIdentityBoth[W, S, R, E]
    : CovariantIdentityBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] =
    new CovariantIdentityBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] {
      def any: ZPure[W, S, S, Any, Nothing, Any]                                                                   =
        ZPure.unit
      def both[A, B](fa: => ZPure[W, S, S, R, E, A], fb: => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)] =
        fa.zip(fb)
      def map[A, B](f: A => B): ZPure[W, S, S, R, E, A] => ZPure[W, S, S, R, E, B]                                 =
        _.map(f)
      override def collectM[A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
        f: A => ZPure[W, S, S, R, E, Option[B]]
      )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]]              =
        ZPure.collect(in)(f)
      override def forEach[A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
      )(f: A => ZPure[W, S, S, R, E, B])(implicit
        bf: BuildFrom[Collection[A], B, Collection[B]]
      ): ZPure[W, S, S, R, E, Collection[B]]                                                                       =
        ZPure.foreach(in)(f)
      override def forEach_[A, B](in: Iterable[A])(
        f: A => ZPure[W, S, S, R, E, Any]
      ): ZPure[W, S, S, R, E, Unit]                                                                                =
        ZPure.foreachDiscard(in)(f)
    }

  /**
   * The `IdentityFlatten` instance for `ZPure`.
   */
  implicit def ZPureIdentityFlatten[W, S, R, E]
    : IdentityFlatten[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] =
    new IdentityFlatten[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] {
      def any: ZPure[W, S, S, Any, Nothing, Any]                                                  =
        ZPure.unit
      def flatten[A](ffa: ZPure[W, S, S, R, E, ZPure[W, S, S, R, E, A]]): ZPure[W, S, S, R, E, A] =
        ffa.flatten
    }

  implicit final class ZPureRefineToOrDieOps[W, S1, S2, R, E <: Throwable, A](self: ZPure[W, S1, S2, R, E, A]) {

    /**
     * Keeps some of the errors, and `throw` the rest.
     */
    def refineToOrDie[E1 <: E: ClassTag](implicit ev: CanFail[E]): ZPure[W, S1, S2, R, E1, A] =
      self.refineOrDie { case e: E1 => e }
  }

  private final case class Succeed[+A](value: A)            extends ZPure[Nothing, Any, Nothing, Any, Nothing, A]
  private final case class Fail[+E](error: E)               extends ZPure[Nothing, Any, Nothing, Any, E, Nothing]
  private final case class Update[-S1, +S2](run0: S1 => S2) extends ZPure[Nothing, S1, S2, Any, Nothing, Unit]
  private final case class FlatMap[+W, -S1, S2, +S3, -R, +E, A, +B](
    value: ZPure[W, S1, S2, R, E, A],
    continue: A => ZPure[W, S2, S3, R, E, B]
  ) extends ZPure[W, S1, S3, R, E, B]
  private final case class FMap[+W, -S1, S2, -R, +E, A, +B](
    value: ZPure[W, S1, S2, R, E, A],
    run0: A => B
  ) extends ZPure[W, S1, S2, R, E, B]
  private final case class Fold[+W, -S1, S2, +S3, -R, E1, +E2, A, +B](
    value: ZPure[W, S1, S2, R, E1, A],
    failure: E1 => ZPure[W, S1, S3, R, E2, B],
    success: A => ZPure[W, S2, S3, R, E2, B]
  ) extends ZPure[W, S1, S3, R, E2, B]
      with Function[A, ZPure[W, S2, S3, R, E2, B]] {
    override def apply(a: A): ZPure[W, S2, S3, R, E2, B] =
      success(a)
  }
  private final case class Environment[W, S1, S2, R, E, A](access: ZEnvironment[R] => A)
      extends ZPure[W, S1, S2, R, E, A]
  private final case class Provide[W, S1, S2, R, E, A](r: ZEnvironment[R], continue: ZPure[W, S1, S2, R, E, A])
      extends ZPure[W, S1, S2, Any, E, A]
  private final case class Log[S, +W](log: W)               extends ZPure[W, S, S, Any, Nothing, Unit]
  private final case class ClearLogOnError[W, S1, S2, R, E, A](
    value: Boolean,
    continue: ZPure[W, S1, S2, R, E, A]
  ) extends ZPure[W, S1, S2, R, E, A]
  private final case class Inspect[S, +A](run0: S => A)     extends ZPure[Nothing, S, S, Any, Nothing, A]

  private object Runner {
    private[this] val pool = new ThreadLocal[(Runner, AtomicBoolean)] {
      override def initialValue(): (Runner, AtomicBoolean) = (new Runner(), new AtomicBoolean(false))
    }

    def apply[W, S1, S2, R, E, A](
      state: S1,
      zPure: ZPure[W, S1, S2, R, E, A]
    ): (Chunk[W], Either[E, (S2, A)]) = {
      val (runner, running) = pool.get()

      if (running.compareAndSet(false, true)) {
        try
          runner.run(state, zPure)
        finally {
          runner.clear()
          running.set(false)
        }
      } else {
        new Runner().run(state, zPure)
      }
    }
  }

  final private class Runner private {
    private type Erased = ZPure[Any, Any, Any, Any, Any, Any]

    private[this] var _environment     = ZEnvironment.empty
    private[this] var _clearLogOnError = false
    private[this] var _logs            = ChunkBuilder.make[Any]()
    private[this] val stack            = new Stack

    private def clear(): Unit = {
      _environment = ZEnvironment.empty
      _clearLogOnError = false
      _logs.clear()
      stack.clear()
    }

    private def run[W, S1, S2, R, E, A](
      state: S1,
      zPure: ZPure[W, S1, S2, R, E, A]
    ): (Chunk[W], Either[E, (S2, A)]) = {
      val result = loop(state, zPure.asInstanceOf[Erased])

      (_logs.result().asInstanceOf[Chunk[W]], result)
    }

    @nowarn("msg=type argument")
    private def loop[S2, E, A](state: Any, zPure: Erased): Either[E, (S2, A)] = {
      // NOTE: Be careful not to add these into a lambda to avoid them being moved to the heap
      var s0: Any  = state
      var a: Any   = null
      var curZPure = zPure

      while (curZPure ne null)
        curZPure match {
          case flatmap0: ZPure.FlatMap[Any, Any, Any, Any, Any, Any, Any, Any] =>
            val nested       = flatmap0.value
            val continuation = flatmap0.continue

            nested match {
              case succeed0: Succeed[Any] =>
                curZPure = continuation(succeed0.value)

              case inspect0: Inspect[Any, Any] =>
                curZPure = continuation(inspect0.run0(s0))

              case update0: Update[Any, Any] =>
                s0 = update0.run0(s0)
                curZPure = continuation(())

              case log0: Log[Any, Any] =>
                _logs addOne log0.log
                curZPure = continuation(())

              case environment0: Environment[Any, Any, Any, Any, Any, Any] =>
                curZPure = continuation(environment0.access(_environment))

              case fmap0: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                curZPure = fmap0.value
                stack.push2(continuation, fmap0)

              case nested =>
                curZPure = nested
                stack.push(continuation)
            }

          case succeed0: Succeed[Any]                         =>
            a = succeed0.value
            var loop = true
            while (loop)
              stack.pop() match {
                case null                                          =>
                  loop = false
                  curZPure = null
                case fmap: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                  a = fmap.run0(a)
                case other                                         =>
                  loop = false
                  curZPure = other.asInstanceOf[Any => Erased](a)
              }
          case fmap0: FMap[Any, Any, Any, Any, Any, Any, Any] =>
            curZPure = fmap0.value
            stack.push(fmap0)

          case fold0: Fold[Any, Any, Any, Any, Any, Any, Any, Any, Any] =>
            val state = s0
            val clear = _clearLogOnError
            val fold  = if (clear) {
              val previousLogs = _logs
              _logs = ChunkBuilder.make()

              ZPure.Fold(
                fold0.value,
                (error: Any) => {
                  _logs = previousLogs
                  ZPure.set(state) *> fold0.failure(error)
                },
                (a: Any) => {
                  val logs0 = _logs.result()
                  if (!logs0.isEmpty) previousLogs ++= logs0
                  _logs = previousLogs
                  fold0.success(a)
                }
              )
            } else {
              ZPure.Fold(
                fold0.value,
                ZPure.set(state) *> fold0.failure(_: Any),
                fold0.success
              )
            }

            stack.push(fold)
            curZPure = fold0.value

          case log0: Log[Any, Any] =>
            _logs addOne log0.log
            a = ()
            var loop = true
            while (loop)
              stack.pop() match {
                case null                                          =>
                  loop = false
                  curZPure = null
                case fmap: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                  a = fmap.run0(a)
                case other                                         =>
                  loop = false
                  curZPure = other.asInstanceOf[Any => Erased](a)
              }

          case provide0: Provide[Any, Any, Any, Any, Any, Any] =>
            val previousEnv = _environment
            _environment = provide0.r
            curZPure = provide0.continue.foldM(
              e => { _environment = previousEnv; ZPure.fail(e) },
              a => { _environment = previousEnv; ZPure.succeed(a) }
            )

          case environment0: Environment[Any, Any, Any, Any, Any, Any] =>
            a = environment0.access(_environment)
            var loop = true
            while (loop)
              stack.pop() match {
                case null                                          =>
                  loop = false
                  curZPure = null
                case fmap: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                  a = fmap.run0(a)
                case other                                         =>
                  loop = false
                  curZPure = other.asInstanceOf[Any => Erased](a)
              }

          case inspect0: Inspect[Any, Any] =>
            a = inspect0.run0(s0)
            var loop = true
            while (loop)
              stack.pop() match {
                case null                                          =>
                  loop = false
                  curZPure = null
                case fmap: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                  a = fmap.run0(a)
                case other                                         =>
                  loop = false
                  curZPure = other.asInstanceOf[Any => Erased](a)
              }

          case modify0: Update[Any, Any] =>
            s0 = modify0.run0(s0)
            a = ()
            var loop = true
            while (loop)
              stack.pop() match {
                case null                                          =>
                  loop = false
                  curZPure = null
                case fmap: FMap[Any, Any, Any, Any, Any, Any, Any] =>
                  a = fmap.run0(a)
                case other                                         =>
                  loop = false
                  curZPure = other.asInstanceOf[Any => Erased](a)
              }

          case flag0: ClearLogOnError[Any, Any, Any, Any, Any, Any] =>
            val oldValue = _clearLogOnError
            _clearLogOnError = flag0.value
            val resetFn  = (a: Any) => { _clearLogOnError = oldValue; a }
            curZPure = flag0.continue.bimap(resetFn, resetFn)

          case fail0: Fail[Any] =>
            curZPure = null
            while (curZPure eq null)
              stack.pop() match {
                case null                                                     =>
                  // No error handlers in the stack, exit evaluation with the error
                  return Left(fail0.error.asInstanceOf[E])
                case value: Fold[Any, Any, Any, Any, Any, Any, Any, Any, Any] =>
                  curZPure = value.failure(fail0.error)
                case _                                                        =>
                  ()
              }
        }

      Right((s0.asInstanceOf[S2], a.asInstanceOf[A]))
    }
  }
}
