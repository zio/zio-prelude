package zio.prelude.fx

import zio.internal.Stack
import zio.prelude._
import zio.test.Assertion
import zio.{CanFail, Chunk, ChunkBuilder, NeedsEnv}

import scala.annotation.{implicitNotFound, switch}
import scala.util.Try
import scala.util.control.NonFatal

/**
 * `ZPure[W, S1, S2, R, E, A]` is a purely functional description of a
 * computation that requires an environment `R` and an initial state `S1` and
 * may either fail with an `E` or succeed with an updated state `S2` and an `A`
 * along with in either case a log with entries of type `W`. Because of its
 * polymorphism `ZPure` can be used to model a variety of effects including
 * context, state, failure, and logging.
 */

sealed trait ZPure[+W, -S1, +S2, -R, +E, +A] { self =>
  import ZPure._

  /**
   * A symbolic alias for `zip`.
   */
  final def &&&[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, (A, B)] =
    self zip that

  /**
   * Splits the environment, providing the first part to this computaiton and
   * the second part to that computation.
   */
  final def ***[W1 >: W, S3, R1, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, (R, R1), E1, (A, B)] =
    (ZPure.first >>> self) &&& (ZPure.second >>> that)

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[W1 >: W, S3, R1 <: R, E1 >: E, B](that: ZPure[W1, S2, S3, R1, E1, B]): ZPure[W1, S1, S3, R1, E1, B] =
    self zipRight that

  /**
   * Runs this computation if the provided environment is a `Left` or else
   * runs that computation if the provided environment is a `Right`, returning
   * the result in an `Either`.
   */
  final def +++[W1 >: W, S0 <: S1, S3 >: S2, R1, B, E1 >: E](
    that: ZPure[W1, S0, S3, R1, E1, B]
  ): ZPure[W1, S0, S3, Either[R, R1], E1, Either[A, B]] =
    ZPure.accessM(_.fold(self.provide(_).map(Left(_)), that.provide(_).map(Right(_))))

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[W1 >: W, S3, R1 <: R, E1 >: E, B](that: ZPure[W1, S2, S3, R1, E1, B]): ZPure[W1, S1, S3, R1, E1, A] =
    self zipLeft that

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, (A, B)] =
    self zip that

  /**
   * A symbolic alias for `orElseEither`.
   */
  final def <+>[W1 >: W, S0 <: S1, S3 >: S2, R1 <: R, E1, B](
    that: => ZPure[W1, S0, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, Either[A, B]] =
    self orElseEither that

  /**
   * A symbolic alias for `compose`.
   */
  final def <<<[W1 >: W, S0, R0, E1 >: E](that: ZPure[W1, S0, S1, R0, E1, R]): ZPure[W1, S0, S2, R0, E1, A] =
    self compose that

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
   * A symbolic alias for `andThen`.
   */
  final def >>>[W1 >: W, S3, E1 >: E, B](that: ZPure[W1, S2, S3, A, E1, B]): ZPure[W1, S1, S3, R, E1, B] =
    self andThen that

  /**
   * A symbolic alias for `log`.
   */
  final def ??[W1 >: W](w: W1): ZPure[W1, S1, S2, R, E, A] =
    self.log(w)

  /**
   * A symbolic alias for `join`.
   */
  final def |||[W1 >: W, S0 <: S1, S3 >: S2, R1, B, E1 >: E, A1 >: A](
    that: ZPure[W1, S0, S3, R1, E1, A1]
  ): ZPure[W1, S0, S3, Either[R, R1], E1, A1] =
    self join that

  /**
   * Submerges the error case of an `Either` into the error type of this
   * computation.
   */
  final def absolve[E1 >: E, B](implicit ev: A <:< Either[E1, B]): ZPure[W, S1, S2, R, E1, B] =
    flatMap(ev(_).fold(fail, succeed))

  /**
   * Runs this computation and uses its result to provide the specified
   * computation with its required environment.
   */
  final def andThen[W1 >: W, S3, E1 >: E, B](that: ZPure[W1, S2, S3, A, E1, B]): ZPure[W1, S1, S3, R, E1, B] =
    self.flatMap(that.provide)

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
   * Runs the specified computation and uses its result to provide this
   * computation with its required environment.
   */
  final def compose[W1 >: W, S0, R0, E1 >: E](that: ZPure[W1, S0, S1, R0, E1, R]): ZPure[W1, S0, S2, R0, E1, A] =
    that andThen self

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
   * or righr function passed to `fold`.
   */
  final def fold[S3 >: S2 <: S1, B](failure: E => B, success: A => B)(implicit
    ev: CanFail[E]
  ): ZPure[W, S3, S3, R, Nothing, B] =
    self.foldM(e => ZPure.succeed(failure(e)), a => ZPure.succeed(success(a)))

  final def foldCauseM[W1 >: W, S0 <: S1, S3, R1 <: R, E1, B](
    failure: Cause[E] => ZPure[W1, S0, S3, R1, E1, B],
    success: A => ZPure[W1, S2, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, B] =
    Fold(self, failure, success)

  /**
   * Recovers from errors by accepting one computation to execute for the case
   * of an error, and one computation to execute for the case of success.
   */
  final def foldM[W1 >: W, S0 <: S1, S3, R1 <: R, E1, B](
    failure: E => ZPure[W1, S0, S3, R1, E1, B],
    success: A => ZPure[W1, S2, S3, R1, E1, B]
  )(implicit ev: CanFail[E]): ZPure[W1, S0, S3, R1, E1, B] =
    foldCauseM((cause: Cause[E]) => failure(cause.first), success)

  /**
   * Exposes the output state into the value channel.
   */
  final def getState: ZPure[W, S1, S2, R, E, (S2, A)] =
    flatMap(a => get.map(s => (s, a)))

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
   * Runs this computation if the provided environment is a `Left` or else
   * runs that computation if the provided environment is a `Right`, unifying
   * the result to a common supertype.
   */
  final def join[W1 >: W, S0 <: S1, S3 >: S2, R1, B, E1 >: E, A1 >: A](
    that: ZPure[W1, S0, S3, R1, E1, A1]
  ): ZPure[W1, S0, S3, Either[R, R1], E1, A1] =
    ZPure.accessM(_.fold(self.provide, that.provide))

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
    flatMap(a => succeed(f(a)))

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
      a => a.fold[ZPure[W, S2, S2, R, Option[E], Unit]](ZPure.succeed(()))(_ => ZPure.fail(None))
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
    orElse(succeed(a1).mapState(_ => s3))

  /**
   * Provides this computation with its required environment.
   */
  final def provide(r: R)(implicit ev: NeedsEnv[R]): ZPure[W, S1, S2, Any, E, A] =
    ZPure.Provide(r, self)

  /**
   * Provides this computation with part of its required environment, leaving
   * the remainder.
   */
  final def provideSome[R0](f: R0 => R): ZPure[W, S1, S2, R0, E, A] =
    ZPure.accessM(r0 => self.provide(f(r0)))

  /**
   * Provides this computation with its initial state.
   */
  final def provideState(s: S1): ZPure[W, Any, S2, R, E, A] =
    set(s) *> self

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
    self.zip(ZPure.get).flatMap { case (a, s) =>
      if (f(s)) ZPure.succeed(a)
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
    runAll(s)._2.fold(cause => ev2(cause.first), identity)

  /**
   * Runs this computation with the specified initial state, returning both the
   * log and either all the failures that occurred or the updated state and the
   * result.
   */
  final def runAll(s: S1)(implicit ev: Any <:< R): (Chunk[W], Either[Cause[E], (S2, A)]) = {
    val _                                                        = ev
    val stack: Stack[Any => ZPure[Any, Any, Any, Any, Any, Any]] = Stack()
    val environments: Stack[AnyRef]                              = Stack()
    var s0: Any                                                  = s
    var a: Any                                                   = null
    val builder: ChunkBuilder[Any]                               = ChunkBuilder.make()
    var failed                                                   = false
    var curZPure: ZPure[Any, Any, Any, Any, Any, Any]            = self.asInstanceOf[ZPure[Any, Any, Any, Any, Any, Any]]

    def findNextErrorHandler(): Unit = {
      var unwinding = true
      while (unwinding)
        stack.pop() match {
          case value: Fold[_, _, _, _, _, _, _, _, _] =>
            val continuation = value.failure
            stack.push(continuation.asInstanceOf[Any => ZPure[Any, Any, Any, Any, Any, Any]])
            unwinding = false
          case null                                   =>
            unwinding = false
          case _                                      =>
        }
    }

    while (curZPure ne null) {
      val tag = curZPure.tag
      (tag: @switch) match {
        case Tags.FlatMap =>
          val zPure        = curZPure.asInstanceOf[FlatMap[Any, Any, Any, Any, Any, Any, Any, Any]]
          val nested       = zPure.value
          val continuation = zPure.continue

          nested.tag match {
            case Tags.Succeed =>
              val zPure2 = nested.asInstanceOf[Succeed[Any]]
              curZPure = continuation(zPure2.value)

            case Tags.Modify =>
              val zPure2 = nested.asInstanceOf[Modify[Any, Any, Any]]

              val updated = zPure2.run0(s0)
              s0 = updated._1
              a = updated._2
              curZPure = continuation(a)

            case _ =>
              curZPure = nested
              stack.push(continuation)
          }

        case Tags.Succeed =>
          val zPure     = curZPure.asInstanceOf[Succeed[Any]]
          a = zPure.value
          val nextInstr = stack.pop()
          if (nextInstr eq null) curZPure = null else curZPure = nextInstr(a)
        case Tags.Fail    =>
          val zPure     = curZPure.asInstanceOf[Fail[Any]]
          findNextErrorHandler()
          val nextInstr = stack.pop()
          if (nextInstr eq null) {
            failed = true
            a = zPure.error
            curZPure = null
          } else
            curZPure = nextInstr(zPure.error)

        case Tags.Fold    =>
          val zPure = curZPure.asInstanceOf[Fold[Any, Any, Any, Any, Any, Any, Any, Any, Any]]
          curZPure = zPure.value
          stack.push(zPure)
        case Tags.Access  =>
          val zPure = curZPure.asInstanceOf[Access[Any, Any, Any, Any, Any, Any]]
          curZPure = zPure.access(environments.peek())
        case Tags.Provide =>
          val zPure = curZPure.asInstanceOf[Provide[Any, Any, Any, Any, Any, Any]]
          environments.push(zPure.r.asInstanceOf[AnyRef])
          curZPure = zPure.continue.foldM(
            e => ZPure.succeed(environments.pop()) *> ZPure.fail(e),
            a => ZPure.succeed(environments.pop()) *> ZPure.succeed(a)
          )
        case Tags.Modify  =>
          val zPure     = curZPure.asInstanceOf[Modify[Any, Any, Any]]
          val updated   = zPure.run0(s0)
          s0 = updated._1
          a = updated._2
          val nextInstr = stack.pop()
          if (nextInstr eq null) curZPure = null else curZPure = nextInstr(a)
        case Tags.Log     =>
          val zPure     = curZPure.asInstanceOf[Log[Any, Any]]
          builder += zPure.log
          val nextInstr = stack.pop()
          a = ()
          if (nextInstr eq null) curZPure = null else curZPure = nextInstr(a)
      }
    }
    val log = builder.result().asInstanceOf[Chunk[W]]
    if (failed) (log, Left(a.asInstanceOf[Cause[E]]))
    else (log, Right((s0.asInstanceOf[S2], a.asInstanceOf[A])))
  }

  /**
   * Runs this computation to produce its result or the first failure to
   * occur.
   */
  final def runEither(implicit ev1: Unit <:< S1, ev2: Any <:< R): Either[E, A] =
    runAll(())._2.fold(cause => Left(cause.first), { case (_, a) => Right(a) })

  /**
   * Runs this computation to produce its result and the log.
   */
  final def runLog(implicit ev1: Unit <:< S1, ev2: Any <:< R, ev3: E <:< Nothing): (Chunk[W], A) = {
    val (log, either) = runAll(())
    (log, either.fold(cause => ev3(cause.first), { case (_, a) => a }))
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
   * Exposes the full cause of failures of this computation.
   */
  final def sandbox: ZPure[W, S1, S2, R, Cause[E], A] =
    foldCauseM(ZPure.fail, ZPure.succeed)

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

  def tag: Int

  /**
   * Submerges the full cause of failures of this computation.
   */
  def unsandbox[E1](implicit ev: E <:< Cause[E1]): ZPure[W, S1, S2, R, E1, A] =
    foldM(e => ZPure.halt(ev(e)), a => ZPure.succeed(a))

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both into a tuple.
   */
  final def zip[W1 >: W, S3, R1 <: R, E1 >: E, B](
    that: ZPure[W1, S2, S3, R1, E1, B]
  ): ZPure[W1, S1, S3, R1, E1, (A, B)] =
    self.zipWith(that)((_, _))

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
    self.zipWith(that)((_, b) => b)

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both using the specified function.
   */
  final def zipWith[W1 >: W, S3, R1 <: R, E1 >: E, B, C](
    that: ZPure[W1, S2, S3, R1, E1, B]
  )(f: (A, B) => C): ZPure[W1, S1, S3, R1, E1, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))

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
  final def unit: ZPure[W, S1, S2, R, E, Unit] = as(())

}

object ZPure {

  implicit final class UnifiedSyntax[W, S, R, E, A](private val self: ZPure[W, S, S, R, E, A]) extends AnyVal {
    def <&>[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)]                      =
      self zipPar that
    def <&[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, A]                            =
      self zipParLeft that
    def &>[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, B]                            =
      self zipParRight that
    def zipPar[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)]                   =
      self.zipWithPar(that)((_, _))
    def zipPar0[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)]                  =
      self.zipWithPar(that)((_, _))
    def zipParLeft[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, A]                    =
      self.zipWithPar(that)((a, _) => a)
    def zipParRight[B](that: ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, B]                   =
      self.zipWithPar(that)((_, b) => b)
    def zipWithPar[B, C](that: ZPure[W, S, S, R, E, B])(f: (A, B) => C): ZPure[W, S, S, R, E, C] =
      self.foldCauseM(
        c1 =>
          that.foldCauseM(
            c2 => ZPure.halt(c1 && c2),
            _ => ZPure.halt(c1)
          ),
        a => that.map(b => f(a, b))
      )
  }

  def access[R]: AccessPartiallyApplied[R] =
    new AccessPartiallyApplied

  def accessM[R]: AccessMPartiallyApplied[R] =
    new AccessMPartiallyApplied

  /**
   * Combines a collection of computations into a single computation that
   * passes the updated state from each computation to the next and collects
   * the results.
   */
  def collectAll[F[+_]: ForEach, W, S, R, E, A](fa: F[ZPure[W, S, S, R, E, A]]): ZPure[W, S, S, R, E, F[A]] =
    ForEach[F].flip[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda, A](fa)

  def environment[S, R]: ZPure[Nothing, S, S, R, Nothing, R]                                                =
    access(r => r)

  def fail[E](e: E): ZPure[Nothing, Any, Nothing, Any, E, Nothing] =
    halt(Cause(e))

  def halt[E](cause: Cause[E]): ZPure[Nothing, Any, Nothing, Any, E, Nothing] =
    ZPure.Fail(cause)

  /**
   * Constructs a computation that extracts the first element of a tuple.
   */
  def first[S, A]: ZPure[Nothing, S, S, (A, Any), Nothing, A] =
    fromFunction(_._1)

  /**
   * Constructs a computation from a value and an assertion about that value.
   * The resulting computation will be a success if the value satisfies the
   * assertion or else will contain a string rendering describing how the
   * value did not satisfy the assertion.
   */
  def fromAssert[S, A](value: A)(assertion: Assertion[A]): ZPure[Nothing, S, S, Any, String, A] =
    if (assertion.test(value)) succeed(value)
    else fail(s"$value did not satisfy ${assertion.render}")

  /**
   * Constructs a computation from an effect that may throw.
   */
  def fromEffect[S, A](effect: => A): ZPure[Nothing, S, S, Any, Throwable, A] =
    suspend {
      try ZPure.succeed(effect)
      catch {
        case NonFatal(e) => ZPure.fail(e)
      }
    }

  /**
   * Constructs a computation from an `Either`.
   */
  def fromEither[S, L, R](either: Either[L, R]): ZPure[Nothing, S, S, Any, L, R] =
    either.fold(l => ZPure.fail(l), r => ZPure.succeed(r))

  /**
   * Constructs a computation from a function.
   */
  def fromFunction[S, R, A](f: R => A): ZPure[Nothing, S, S, R, Nothing, A] =
    access(f)

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
    fromEffect(t).flatMap {
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
  ): ZPure[W, S, S, R, E, F[B]]                     =
    ForEach[F].forEach[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda, A, B](fa)(f)

  /**
   * Constructs a computation that returns the initial state unchanged.
   */
  def get[S]: ZPure[Nothing, S, S, Any, Nothing, S] =
    modify(s => (s, s))

  def log[S, W](w: W): ZPure[W, S, S, Any, Nothing, Unit] =
    ZPure.Log(w)

  /**
   * Constructs a computation from the specified modify function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): ZPure[Nothing, S1, S2, Any, Nothing, A] =
    Modify(f)

  /**
   * Constructs a computation that may fail from the specified modify function.
   */
  def modifyEither[S1, S2, E, A](f: S1 => Either[E, (S2, A)]): ZPure[Nothing, S1, S2, Any, E, A] =
    get.map(f).flatMap {
      case Left(e)        => ZPure.fail(e)
      case Right((s2, a)) => ZPure.succeed(a).asState(s2)
    }

  /**
   * Constructs a computation that extracts the second element of a tuple.
   */
  def second[S, B]: ZPure[Nothing, S, S, (Any, B), Nothing, B] =
    fromFunction(_._2)

  /**
   * Constructs a computation that sets the state to the specified value.
   */
  def set[S](s: S): ZPure[Nothing, Any, S, Any, Nothing, Unit] =
    modify(_ => (s, ()))

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
    succeed(())

  /**
   * Constructs a computation from the specified update function.
   */
  def update[S1, S2](f: S1 => S2): ZPure[Nothing, S1, S2, Any, Nothing, Unit] =
    modify(s => (f(s), ()))

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[S, A](f: R => A): ZPure[Nothing, S, S, R, Nothing, A] =
      Access(r => succeed(f(r)))
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[W, S1, S2, E, A](f: R => ZPure[W, S1, S2, Any, E, A]): ZPure[W, S1, S2, R, E, A] =
      Access(f)
  }

  @implicitNotFound(
    "Pattern guards are only supported when the error type is a supertype of NoSuchElementException. However, your effect has ${E} for the error type."
  )
  abstract class CanFilter[+E] {
    def apply(t: NoSuchElementException): E
  }

  object CanFilter {
    implicit def canFilter[E >: NoSuchElementException]: CanFilter[E] =
      new CanFilter[E] {
        def apply(t: NoSuchElementException): E = t
      }
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
  implicit def ZPureIdentityBoth[W, S, R, E]: IdentityBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] {
      def any: ZPure[W, S, S, Any, Nothing, Any]                                                                   =
        ZPure.unit
      def both[A, B](fa: => ZPure[W, S, S, R, E, A], fb: => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)] =
        fa.zip(fb)
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

  implicit final class ZPureWithFilterOps[W, S1, S2, R, E, A](private val self: ZPure[W, S1, S2, R, E, A])
      extends AnyVal {

    /**
     * Enables to check conditions in the value produced by ZPure
     * If the condition is not satisfied, it fails with NoSuchElementException
     * this provide the syntax sugar in for-comprehension:
     * for {
     *   (i, j) <- zpure1
     *   positive <- zpure2 if positive > 0
     *  } yield ()
     */
    def withFilter(predicate: A => Boolean)(implicit ev: CanFilter[E]): ZPure[W, S1, S2, R, E, A] =
      self.flatMap { a =>
        if (predicate(a)) ZPure.succeed(a)
        else ZPure.fail(ev(new NoSuchElementException("The value doesn't satisfy the predicate")))
      }
  }

  object Tags {
    final val FlatMap = 0
    final val Succeed = 1
    final val Fail    = 2
    final val Fold    = 3
    final val Access  = 4
    final val Provide = 5
    final val Modify  = 6
    final val Log     = 7
  }

  private final case class Succeed[+A](value: A)                     extends ZPure[Nothing, Any, Nothing, Any, Nothing, A] {
    override def tag: Int = Tags.Succeed
  }
  private final case class Fail[+E](error: Cause[E])                 extends ZPure[Nothing, Any, Nothing, Any, E, Nothing] {
    override def tag: Int = Tags.Fail
  }
  private final case class Modify[-S1, +S2, +A](run0: S1 => (S2, A)) extends ZPure[Nothing, S1, S2, Any, Nothing, A]       {
    override def tag: Int = Tags.Modify
  }
  private final case class FlatMap[+W, -S1, S2, +S3, -R, +E, A, +B](
    value: ZPure[W, S1, S2, R, E, A],
    continue: A => ZPure[W, S2, S3, R, E, B]
  )                                                                  extends ZPure[W, S1, S3, R, E, B]                     {
    override def tag: Int = Tags.FlatMap
  }
  private final case class Fold[+W, -S1, S2, +S3, -R, E1, +E2, A, +B](
    value: ZPure[W, S1, S2, R, E1, A],
    failure: Cause[E1] => ZPure[W, S1, S3, R, E2, B],
    success: A => ZPure[W, S2, S3, R, E2, B]
  )                                                                  extends ZPure[W, S1, S3, R, E2, B]
      with Function[A, ZPure[W, S2, S3, R, E2, B]] {
    override def tag: Int                                = Tags.Fold
    override def apply(a: A): ZPure[W, S2, S3, R, E2, B] =
      success(a)
  }
  private final case class Access[W, S1, S2, R, E, A](access: R => ZPure[W, S1, S2, R, E, A])
      extends ZPure[W, S1, S2, R, E, A]            {
    override def tag: Int = Tags.Access
  }
  private final case class Provide[W, S1, S2, R, E, A](r: R, continue: ZPure[W, S1, S2, R, E, A])
      extends ZPure[W, S1, S2, Any, E, A]          {
    override def tag: Int = Tags.Provide
  }

  private final case class Log[S, +W](log: W) extends ZPure[W, S, S, Any, Nothing, Unit] {
    override def tag: Int = Tags.Log
  }
}

trait LowPriorityZPureImplicits {

  /**
   * The `CommutativeBoth` instance for `ZPure`.
   */
  implicit def ZPureCommutativeBoth[W, S, R, E]
    : CommutativeBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] =
    new CommutativeBoth[({ type lambda[+A] = ZPure[W, S, S, R, E, A] })#lambda] {
      def both[A, B](fa: => ZPure[W, S, S, R, E, A], fb: => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, (A, B)] =
        fa.zipPar(fb)
    }
}
