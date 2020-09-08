package zio.prelude.fx

import scala.annotation.switch

import zio.internal.Stack
import zio.prelude._

/**
 * `ZPure[S1, S2, R, E, A]` is a purely functional description of a computation
 * that requires an environment `R` and an initial state `S1` and may either
 * fail with an `E` or succeed with an updated state `S2` and an `A`. Because
 * of its polymorphism `ZPure` can be used to model a variety of effects
 * including context, state, and failure.
 */

sealed trait ZPure[-S1, +S2, -R, +E, +A] { self =>
  import ZPure._

  /**
   * A symbolic alias for `zip`.
   */
  final def &&&[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, (A, B)] =
    self zip that

  /**
   * A symbolic alias for `zip`.
   */
  final def ***[S3, R1, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, (R, R1), E1, (A, B)] =
    (ZPure.first >>> self) &&& (ZPure.second >>> that)

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    self zipRight that

  /**
   * Runs this computatation if the provided environment is a `Left` or else
   * runs that computation if the provided environment is a `Right`, returning
   * the result in an `Either`.
   */
  final def +++[S0 <: S1, S3 >: S2, R1, B, E1 >: E](
    that: ZPure[S0, S3, R1, E1, B]
  ): ZPure[S0, S3, Either[R, R1], E1, Either[A, B]] =
    ZPure.accessM(_.fold(self.provide(_).map(Left(_)), that.provide(_).map(Right(_))))

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, A] =
    self zipLeft that

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, (A, B)] =
    self zip that

  /**
   * A symbolic alias for `orElseEither`.
   */
  final def <+>[S0 <: S1, S3 >: S2, R1 <: R, E1, B](
    that: => ZPure[S0, S3, R1, E1, B]
  ): ZPure[S0, S3, R1, E1, Either[A, B]] =
    self orElseEither that

  /**
   * A symbolic alias for `compose`.
   */
  final def <<<[S0, R0, E1 >: E](that: ZPure[S0, S1, R0, E1, R]): ZPure[S0, S2, R0, E1, A] =
    self compose that

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>[S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    that: => ZPure[S0, S3, R1, E1, A1]
  ): ZPure[S0, S3, R1, E1, A1] =
    self orElse that

  /**
   * A symbolic alias for `flatMap`.
   */
  final def >>=[S3, R1 <: R, E1 >: E, B](f: A => ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    self flatMap f

  /**
   * A symbolic alias for `andThen`.
   */
  final def >>>[S3, E1 >: E, B](that: ZPure[S2, S3, A, E1, B]): ZPure[S1, S3, R, E1, B] =
    self andThen that

  /**
   * Runs this computatation if the provided environment is a `Left` or else
   * runs that computation if the provided environment is a `Right`, unifying
   * the result to a common supertype.
   */
  final def |||[S0 <: S1, S3 >: S2, R1, B, E1 >: E, A1 >: A](
    that: ZPure[S0, S3, R1, E1, A1]
  ): ZPure[S0, S3, Either[R, R1], E1, A1] =
    ZPure.accessM(_.fold(self.provide, that.provide))

  /**
   * Submerges the error case of an `Either` into the error type of this
   * computation.
   */
  final def absolve[E1 >: E, B](implicit ev: A <:< Either[E1, B]): ZPure[S1, S2, R, E1, B] =
    flatMap(ev(_).fold(fail, succeed))

  /**
   * Runs this computation and uses its result to provide the specified
   * computation with its required environment.
   */
  final def andThen[S3, E1 >: E, B](that: ZPure[S2, S3, A, E1, B]): ZPure[S1, S3, R, E1, B] =
    self.flatMap(that.provide)

  /**
   * Maps the success value of this computation to a constant value.
   */
  final def as[B](b: => B): ZPure[S1, S2, R, E, B] =
    map(_ => b)

  /**
   * Maps the success value of this computation to the optional value.
   */
  final def asSome: ZPure[S1, S2, R, E, Option[A]] =
    map(Some(_))

  /**
   * Maps the error value of this computation to the optional value.
   */
  final def asSomeError: ZPure[S1, S2, R, Option[E], A] =
    mapError(Some(_))

  /**
   * Returns a computation whose error and success channels have been mapped
   * by the specified functions, `f` and `g`.
   */
  final def bimap[E1, B](f: E => E1, g: A => B): ZPure[S1, S2, R, E1, B] =
    foldM(e => fail(f(e)), a => succeed(g(a)))

  /**
   * Recovers from all errors.
   */
  final def catchAll[S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    f: E => ZPure[S0, S3, R1, E1, A1]
  ): ZPure[S0, S3, R1, E1, A1] =
    foldM(f, succeed)

  /**
   * Recovers from some or all of the error cases.
   */
  final def catchSome[S0 <: S1, S3 >: S2, R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[E, ZPure[S0, S3, R1, E1, A1]]
  ): ZPure[S0, S3, R1, E1, A1] =
    catchAll(pf.applyOrElse[E, ZPure[S0, S3, R1, E1, A1]](_, fail))

  /**
   * Transforms the result of this computation with the specified partial
   * function, failing with the `e` value if the partial function is not
   * defined for the given input.
   */
  final def collect[E1 >: E, B](e: => E1)(pf: PartialFunction[A, B]): ZPure[S1, S2, R, E1, B] =
    collectM(e)(pf.andThen(succeed(_)))

  /**
   * Transforms the result of this computation with the specified partial
   * function which returns a new computation, failing with the `e` value if
   * the partial function is not defined for the given input.
   */
  final def collectM[S3, R1 <: R, E1 >: E, B](
    e: => E1
  )(pf: PartialFunction[A, ZPure[S2, S3, R1, E1, B]]): ZPure[S1, S3, R1, E1, B] =
    flatMap(pf.applyOrElse[A, ZPure[S2, S3, R1, E1, B]](_, _ => fail(e)))

  /**
   * Runs the specified computation and uses its result to provide this
   * computation with its required environment.
   */
  final def compose[S0, R0, E1 >: E](that: ZPure[S0, S1, R0, E1, R]): ZPure[S0, S2, R0, E1, A] =
    that andThen self

  /**
   * Transforms the initial state of this computation with the specified
   * function.
   */
  final def contramapState[S0](f: S0 => S1): ZPure[S0, S2, R, E, A] =
    update(f) *> self

  /**
   * Returns a computation whose failure and success have been lifted into an
   * `Either`. The resulting computation cannot fail, because the failure case
   * has been exposed as part of the `Either` success case.
   */
  final def either[S3 >: S2 <: S1]: ZPure[S3, S3, R, Nothing, Either[E, A]] =
    fold(Left(_), Right(_))

  /**
   * Extends this computation with another computation that depends on the
   * result of this computation by running the first computation, using its
   * result to generate a second computation, and running that computation.
   */
  final def flatMap[S3, R1 <: R, E1 >: E, B](f: A => ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    FlatMap(self, f)

  /**
   * Flattens a nested computation to a single computation by running the outer
   * computation and then running the inner computation.
   */
  final def flatten[S3, R1 <: R, E1 >: E, B](implicit ev: A <:< ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    flatMap(ev)

  /**
   * Swaps the error and success types of this computation.
   */
  final def flip[S3 >: S2 <: S1]: ZPure[S3, S3, R, A, E] =
    foldM(succeed, fail)

  /**
   * Folds over the failed or successful results of this computation to yield
   * a computation that does not fail, but succeeds with the value of the left
   * or righr function passed to `fold`.
   */
  final def fold[S3 >: S2 <: S1, B](failure: E => B, success: A => B): ZPure[S3, S3, R, Nothing, B] =
    self.foldM(e => ZPure.succeed(failure(e)), a => ZPure.succeed(success(a)))

  /**
   * Recovers from errors by accepting one computation to execute for the case
   * of an error, and one computation to execute for the case of success.
   */
  final def foldM[S0 <: S1, S3, R1 <: R, E1, B](
    failure: E => ZPure[S0, S3, R1, E1, B],
    success: A => ZPure[S2, S3, R1, E1, B]
  ): ZPure[S0, S3, R1, E1, B] =
    Fold(self, failure, success)

  /**
   * Transforms the result of this computation with the specified function.
   */
  final def map[B](f: A => B): ZPure[S1, S2, R, E, B] =
    flatMap(a => succeed(f(a)))

  /**
   * Transforms the error type of this computation with the specified
   * function.
   */
  final def mapError[E1](f: E => E1): ZPure[S1, S2, R, E1, A] =
    catchAll(e => fail(f(e)))

  /**
   * Transforms the updated state of this computation with the specified
   * function.
   */
  final def mapState[S3](f: S2 => S3): ZPure[S1, S3, R, E, A] =
    self <* update(f)

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise executes the specified computation.
   */
  final def orElse[S0 <: S1, S3 >: S2, R1 <: R, E1, A1 >: A](
    that: => ZPure[S0, S3, R1, E1, A1]
  ): ZPure[S0, S3, R1, E1, A1] =
    foldM(_ => that, succeed)

  /**
   * Executes this computation and returns its value, if it succeeds, but
   * otherwise executes the specified computation.
   */
  final def orElseEither[S0 <: S1, S3 >: S2, R1 <: R, E1, B](
    that: => ZPure[S0, S3, R1, E1, B]
  ): ZPure[S0, S3, R1, E1, Either[A, B]] =
    foldM(_ => that.map(Right(_)), a => succeed(Left(a)))

  /**
   * Provides this computation with its required environment.
   */
  final def provide(r: R): ZPure[S1, S2, Any, E, A] =
    ZPure.Provide(r, self)

  /**
   * Runs this computation with the specified initial state, returning both
   * the updated state and the result.
   */
  final def run(s: S1)(implicit ev1: Any <:< R, ev2: E <:< Nothing): (S2, A) =
    runEither(s).fold(ev2, identity)

  /**
   * Runs this computation with the specified initial state, returning either a
   * failure or the updated state and the result
   */
  final def runEither(s: S1)(implicit ev: Any <:< R): Either[E, (S2, A)] = {
    val _                                                   = ev
    val stack: Stack[Any => ZPure[Any, Any, Any, Any, Any]] = Stack()
    var s0: Any                                             = s
    var a: Any                                              = null
    var r: Any                                              = null
    var failed                                              = false
    var curZPure: ZPure[Any, Any, Any, Any, Any]            = self.asInstanceOf[ZPure[Any, Any, Any, Any, Any]]

    def findNextErrorHandler(): Unit = {
      var unwinding = true
      while (unwinding)
        stack.pop() match {
          case value: Fold[_, _, _, _, _, _, _, _] =>
            val continuation = value.failure
            stack.push(continuation.asInstanceOf[Any => ZPure[Any, Any, Any, Any, Any]])
            unwinding = false
          case null                                =>
            unwinding = false
          case _                                   =>
        }
    }

    while (curZPure ne null) {
      val tag = curZPure.tag
      (tag: @switch) match {
        case Tags.FlatMap =>
          val zPure        = curZPure.asInstanceOf[FlatMap[Any, Any, Any, Any, Any, Any, Any]]
          val nested       = zPure.value
          val continuation = zPure.continue

          (nested.tag: @switch) match {
            case Tags.Succeed =>
              val zPure2 = nested.asInstanceOf[Succeed[Any]]
              curZPure = continuation(zPure2.value)

            case Tags.Modify  =>
              val zPure2 = nested.asInstanceOf[Modify[Any, Any, Any, Any]]

              val updated = zPure2.run(s0)
              s0 = updated._1
              a = updated._2
              curZPure = continuation(a)

            case _            =>
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
          val zPure = curZPure.asInstanceOf[Fold[Any, Any, Any, Any, Any, Any, Any, Any]]
          curZPure = zPure.value
          stack.push(zPure)
        case Tags.Access  =>
          val zPure = curZPure.asInstanceOf[Access[Any, Any, Any, Any, Any]]
          curZPure = zPure.access(r)
        case Tags.Provide =>
          val zPure = curZPure.asInstanceOf[Provide[Any, Any, Any, Any, Any]]
          r = zPure.r
          curZPure = zPure.continue
        case Tags.Modify  =>
          val zPure     = curZPure.asInstanceOf[Modify[Any, Any, Any, Any]]
          val updated   = zPure.run(s0)
          s0 = updated._1
          a = updated._2
          val nextInstr = stack.pop()
          if (nextInstr eq null) curZPure = null else curZPure = nextInstr(a)
      }
    }
    if (failed) Left(a.asInstanceOf[E])
    else Right((s0.asInstanceOf[S2], a.asInstanceOf[A]))
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

  def tag: Int

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both into a tuple.
   */
  final def zip[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, (A, B)] =
    self.zipWith(that)((_, _))

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and returning the
   * result of this computation.
   */
  final def zipLeft[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, A] =
    self.zipWith(that)((a, _) => a)

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and returning the
   * result of that computation.
   */
  final def zipRight[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    self.zipWith(that)((_, b) => b)

  /**
   * Combines this computation with the specified computation, passing the
   * updated state from this computation to that computation and combining the
   * results of both using the specified function.
   */
  final def zipWith[S3, R1 <: R, E1 >: E, B, C](
    that: ZPure[S2, S3, R1, E1, B]
  )(f: (A, B) => C): ZPure[S1, S3, R1, E1, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))
}

object ZPure {

  def access[R]: AccessPartiallyApplied[R] =
    new AccessPartiallyApplied

  def accessM[R]: AccessMPartiallyApplied[R] =
    new AccessMPartiallyApplied

  /**
   * Combines a collection of computations into a single computation that
   * passes the updated state from each computation to the next and collects
   * the results.
   */
  def collectAll[F[+_]: Traversable, S, R, E, A](fa: F[ZPure[S, S, R, E, A]]): ZPure[S, S, R, E, F[A]] =
    Traversable[F].flip(fa)

  def environment[S, R]: ZPure[S, S, R, Nothing, R] =
    access(r => r)

  def fail[E](e: E): ZPure[Any, Nothing, Any, E, Nothing] =
    ZPure.Fail(e)

  /**
   * Constructs a computation that extracts the first element of a tuple.
   */
  def first[S, A]: ZPure[S, S, (A, Any), Nothing, A] =
    fromFunction(_._1)

  /**
   * Constructs a computation from a function.
   */
  def fromFunction[S, R, A](f: R => A): ZPure[S, S, R, Nothing, A] =
    access(f)

  /**
   * Maps each element of a collection to a computation and combines them all
   * into a single computation that passes the updated state from each
   * computation to the next and collects the results.
   */
  def foreach[F[+_]: Traversable, S, R, E, A, B](fa: F[A])(f: A => ZPure[S, S, R, E, B]): ZPure[S, S, R, E, F[B]] =
    Traversable[F].foreach(fa)(f)

  /**
   * Constructs a computation that returns the initial state unchanged.
   */
  def get[S]: ZPure[S, S, Any, Nothing, S] =
    modify(s => (s, s))

  /**
   * Constructs a computation from the specified modify function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): ZPure[S1, S2, Any, Nothing, A] =
    Modify(f)

  /**
   * Constructs a computation that extracts the second element of a tuple.
   */
  def second[S, B]: ZPure[S, S, (Any, B), Nothing, B] =
    fromFunction(_._2)

  /**
   * Constructs a computation that sets the state to the specified value.
   */
  def set[S](s: S): ZPure[Any, S, Any, Nothing, Unit] =
    modify(_ => (s, ()))

  /**
   * Constructs a computation that always succeeds with the specified value,
   * passing the state through unchanged.
   */
  def succeed[S, A](a: A): ZPure[S, S, Any, Nothing, A] =
    Succeed(a)

  /**
   * Constructs a computation that always returns the `Unit` value, passing the
   * state through unchanged.
   */
  def unit[S]: ZPure[S, S, Any, Nothing, Unit] =
    succeed(())

  /**
   * Constructs a computation from the specified update function.
   */
  def update[S1, S2](f: S1 => S2): ZPure[S1, S2, Any, Nothing, Unit] =
    modify(s => (f(s), ()))

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[S, A](f: R => A): ZPure[S, S, R, Nothing, A] =
      Access(r => succeed(f(r)))
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[S1, S2, E, A](f: R => ZPure[S1, S2, Any, E, A]): ZPure[S1, S2, R, E, A] =
      Access(f)
  }

  /**
   * The `AssociativeBoth` instance for `ZPure`.
   */
  implicit def ZPureAssociativeBoth[S, R, E]: AssociativeBoth[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] =
    new AssociativeBoth[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] {
      def both[A, B](fa: => ZPure[S, S, R, E, A], fb: => ZPure[S, S, R, E, B]): ZPure[S, S, R, E, (A, B)] =
        fa.zip(fb)
    }

  /**
   * The `Covariant` instance for `ZPure`.
   */
  implicit def ZPureCovariant[S1, S2, R, E]: Covariant[({ type lambda[+A] = ZPure[S1, S2, R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = ZPure[S1, S2, R, E, A] })#lambda] {
      def map[A, B](f: A => B): ZPure[S1, S2, R, E, A] => ZPure[S1, S2, R, E, B] =
        _.map(f)
    }

  /**
   * The `IdentityBoth` instance for `ZPure`.
   */
  implicit def ZPureIdentityBoth[S, R, E]: IdentityBoth[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] {
      def any: ZPure[S, S, Any, Nothing, Any]                                                             =
        ZPure.unit
      def both[A, B](fa: => ZPure[S, S, R, E, A], fb: => ZPure[S, S, R, E, B]): ZPure[S, S, R, E, (A, B)] =
        fa.zip(fb)
    }

  /**
   * The `IdentityFlatten` instance for `ZPure`.
   */
  implicit def ZPureIdentityFlatten[S, R, E]: IdentityFlatten[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] =
    new IdentityFlatten[({ type lambda[+A] = ZPure[S, S, R, E, A] })#lambda] {
      def any: ZPure[S, S, Any, Nothing, Any]                                            =
        ZPure.unit
      def flatten[A](ffa: ZPure[S, S, R, E, ZPure[S, S, R, E, A]]): ZPure[S, S, R, E, A] =
        ffa.flatten
    }

  object Tags {
    final val FlatMap = 0
    final val Succeed = 1
    final val Fail    = 2
    final val Fold    = 3
    final val Access  = 4
    final val Provide = 5
    final val Modify  = 6
  }

  private final case class Succeed[+A](value: A)                        extends ZPure[Any, Nothing, Any, Nothing, A] {
    override def tag: Int = Tags.Succeed
  }
  private final case class Fail[+E](error: E)                           extends ZPure[Any, Nothing, Any, E, Nothing] {
    override def tag: Int = Tags.Fail
  }
  private final case class Modify[-S1, +S2, +E, +A](run: S1 => (S2, A)) extends ZPure[S1, S2, Any, E, A]             {
    override def tag: Int = Tags.Modify
  }
  private final case class FlatMap[-S1, S2, +S3, -R, +E, A, +B](
    value: ZPure[S1, S2, R, E, A],
    continue: A => ZPure[S2, S3, R, E, B]
  )                                                                     extends ZPure[S1, S3, R, E, B]               {
    override def tag: Int = Tags.FlatMap
  }
  private final case class Fold[-S1, S2, +S3, -R, E1, +E2, A, +B](
    value: ZPure[S1, S2, R, E1, A],
    failure: E1 => ZPure[S1, S3, R, E2, B],
    success: A => ZPure[S2, S3, R, E2, B]
  )                                                                     extends ZPure[S1, S3, R, E2, B]
      with Function[A, ZPure[S2, S3, R, E2, B]] {
    override def tag: Int                             = Tags.Fold
    override def apply(a: A): ZPure[S2, S3, R, E2, B] =
      success(a)
  }
  private final case class Access[S1, S2, R, E, A](access: R => ZPure[S1, S2, R, E, A]) extends ZPure[S1, S2, R, E, A] {
    override def tag: Int = Tags.Access
  }
  private final case class Provide[S1, S2, R, E, A](r: R, continue: ZPure[S1, S2, R, E, A])
      extends ZPure[S1, S2, Any, E, A]          {
    override def tag: Int = Tags.Provide
  }
}
