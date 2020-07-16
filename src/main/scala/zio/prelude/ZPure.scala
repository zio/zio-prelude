package zio.prelude

import scala.annotation.switch

import zio.internal.Stack
import zio.prelude.ZPure._

/**
 * `ZPure[S1, S2, E, A]` is a purely functional description of a state transition
 * function that, given an initial state of type `S1`, returns an updated state
 * of type `S2` along with a value of type `A`. State can be used to model
 * computations that maintain state in a purely functional way, automatically
 * handling the bookkeeping of threading the state through the computation.
 */
sealed trait ZPure[-S1, +S2, -R, +E, +A] { self =>

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    self zipRight that

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
   * Transforms the initial state of this state transition function with the
   * specified function.
   */
  final def contramap[S0](f: S0 => S1): ZPure[S0, S2, R, E, A] =
    update(f) *> self

  /**
   * Extends this state transition function with another state transition
   * function that depends on the result of this state transition function by
   * running the first state transition function, using its result to generate
   * a second state transition function, and running that state transition
   * function with the updated state.
   */
  final def flatMap[S3, R1 <: R, E1 >: E, B](f: A => ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    FlatMap(self, f)

  /**
   * Flattens a nested state transition function to a single state transition
   * function by running the outer state transition function and then running
   * the inner state transition function with the updated state.
   */
  final def flatten[S3, R1 <: R, E1 >: E, B](implicit ev: A <:< ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    flatMap(ev)

  /**
   * Transforms the result of this state transition function with the
   * specified function.
   */
  final def map[B](f: A => B): ZPure[S1, S2, R, E, B] =
    flatMap(a => ZPure.succeed(f(a)))

  /**
   * Transforms the updated state of this state transition function with the
   * specified function.
   */
  final def mapState[S3](f: S2 => S3): ZPure[S1, S3, R, E, A] =
    self <* update(f)

  final def run(s: S1)(implicit ev: E <:< Nothing): (S2, A) =
    runEither(s).fold(ev, identity)

  /**
   * Runs this state transition function with the specified initial state,
   * returning both the updated state and the result.
   */
  final def runEither(s: S1): Either[E, (S2, A)] = {
    val stack: Stack[Any => ZPure[Any, Any, Any, Any, Any]] = Stack()
    var s0: Any                                             = s
    var a: Any                                              = null
    var r: Any                                              = null
    var failed                                              = false
    var curState: ZPure[Any, Any, Any, Any, Any]            = self.asInstanceOf[ZPure[Any, Any, Any, Any, Any]]

    def findNextErrorHandler(): Unit = {
      var unwinding = true
      while (unwinding) {
        val nextInstr = stack.pop()
        if (nextInstr.isInstanceOf[Fold[_, _, _, _, _, _, _, _]]) {
          val continuation = nextInstr.asInstanceOf[Fold[_, _, _, _, _, _, _, _]].failure
          stack.push(continuation.asInstanceOf[Any => ZPure[Any, Any, Any, Any, Any]])
          unwinding = false
        } else if (nextInstr eq null) {
          unwinding = false
        }
      }
    }

    while (curState ne null) {
      val tag = curState.tag
      (tag: @switch) match {
        case Tags.FlatMap =>
          val state        = curState.asInstanceOf[FlatMap[Any, Any, Any, Any, Any, Any, Any]]
          val nested       = state.value
          val continuation = state.continue

          (nested.tag: @switch) match {
            case Tags.Succeed =>
              val state2 = nested.asInstanceOf[Succeed[Any, Any]]
              curState = continuation(state2.value)

            case Tags.Modify =>
              val state2 = nested.asInstanceOf[Modify[Any, Any, Any, Any]]

              val updated = state2.run(s0)
              s0 = updated._1
              a = updated._2
              curState = continuation(a)

            case _ =>
              curState = nested
              stack.push(continuation)
          }

        case Tags.Succeed =>
          val state = curState.asInstanceOf[Succeed[Any, Any]]
          a = state.value
          val nextInstr = stack.pop()
          if (nextInstr eq null) curState = null else curState = nextInstr(a)
        case Tags.Fail =>
          val state = curState.asInstanceOf[Fail[Any, Any]]
          findNextErrorHandler()
          val nextInstr = stack.pop()
          if (nextInstr eq null) {
            failed = true
            a = state.error
            curState = null
          } else {
            curState = nextInstr(state.error)
          }

        case Tags.Fold =>
          val state = curState.asInstanceOf[Fold[Any, Any, Any, Any, Any, Any, Any, Any]]
          curState = state.value
          stack.push(state)
        case Tags.Access =>
          val state = curState.asInstanceOf[Access[Any, Any, Any, Any, Any]]
          curState = state.access(r)
        case Tags.Provide =>
          val state = curState.asInstanceOf[Provide[Any, Any, Any, Any, Any]]
          r = state.r
          curState = state.continue
        case Tags.Modify =>
          val state   = curState.asInstanceOf[Modify[Any, Any, Any, Any]]
          val updated = state.run(s0)
          s0 = updated._1
          a = updated._2
          val nextInstr = stack.pop()
          if (nextInstr eq null) curState = null else curState = nextInstr(a)
      }
    }
    if (failed) Left(a.asInstanceOf[E])
    else Right((s0.asInstanceOf[S2], a.asInstanceOf[A]))
  }

  /**
   * Runs this state transition function with the specified initial state,
   * returning the result and discarding the updated state.
   */
  final def runResult(s: S1)(implicit ev: E <:< Nothing): A =
    run(s)._2

  /**
   * Runs this state transition function with the specified initial state,
   * returning the updated state and discarding the result.
   */
  final def runState(s: S1)(implicit ev: E <:< Nothing): S2 =
    run(s)._1

  def tag: Int

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both into a tuple.
   */
  final def zip[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, (A, B)] =
    self.zipWith(that)((_, _))

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * this state transition function.
   */
  final def zipLeft[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, A] =
    self.zipWith(that)((a, _) => a)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * that state transition function.
   */
  final def zipRight[S3, R1 <: R, E1 >: E, B](that: ZPure[S2, S3, R1, E1, B]): ZPure[S1, S3, R1, E1, B] =
    self.zipWith(that)((_, b) => b)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both using the specified function.
   */
  final def zipWith[S3, R1 <: R, E1 >: E, B, C](
    that: ZPure[S2, S3, R1, E1, B]
  )(f: (A, B) => C): ZPure[S1, S3, R1, E1, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))
}

object ZPure {

  /**
   * Constructs a state transition function from a function that given an
   * initial state returns an updated state and a value.
   */
  def apply[S1, S2, A](run: S1 => (S2, A)): ZPure[S1, S2, Any, Nothing, A] =
    modify(run)

  /**
   * Combines a collection of state transition function into a single state
   * transition function that passes the updated state from each state
   * transition function to the next and collects the results.
   */
  def collectAll[F[+_]: Traversable, S, A](fa: F[State[S, A]]): State[S, F[A]] =
    Traversable[F].flip(fa)

  /**
   * Maps each element of a collection to a state transition function and
   * combines them all into a single state transition function that passes the
   * updated state from each state transition function to the next and collects
   * the results.
   */
  def foreach[F[+_]: Traversable, S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    Traversable[F].mapEffect(fa)(f)

  /**
   * Constructs a state transition function that returns the state.
   */
  def get[S]: State[S, S] =
    modify(s => (s, s))

  /**
   * Constructs a state transition function from the specified modify
   * function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): ZPure[S1, S2, Any, Nothing, A] =
    Modify(f)

  /**
   * Constructs a state transition function that sets the state to the
   * specified value.
   */
  def set[S](s: S): ZPure[Any, S, Any, Nothing, Unit] =
    modify(_ => (s, ()))

  /**
   * Constructs a state transition function that always returns the specified
   * value, passing the state through unchanged.
   */
  def succeed[S, A](a: A): State[S, A] =
    Succeed(a)

  /**
   * Constructs a state transition function that always returns the `Unit`
   * value, passing the state through unchanged.
   */
  def unit[S]: State[S, Unit] =
    succeed(())

  /**
   * Constructs a state transition function from the specified update
   * function.
   */
  def update[S1, S2](f: S1 => S2): ZPure[S1, S2, Any, Nothing, Unit] =
    modify(s => (f(s), ()))

  /**
   * The `AssociativeBoth` instance for `State`.
   */
  implicit def StateAssociativeBoth[S]: AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        fa.zip(fb)
    }

  /**
   * The `Contravariant` instance for `State`.
   */
  implicit def StateContravariant[S2, R, E, A]: Contravariant[({ type lambda[-S1] = ZPure[S1, S2, R, E, A] })#lambda] =
    new Contravariant[({ type lambda[-S1] = ZPure[S1, S2, R, E, A] })#lambda] {
      def contramap[S1, S0](f: S0 => S1): ZPure[S1, S2, R, E, A] => ZPure[S0, S2, R, E, A] =
        _.contramap(f)
    }

  /**
   * The `Covariant` instance for `State`.
   */
  implicit def StateCovariant[S1, S2, R, E]: Covariant[({ type lambda[+A] = ZPure[S1, S2, R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = ZPure[S1, S2, R, E, A] })#lambda] {
      def map[A, B](f: A => B): ZPure[S1, S2, R, E, A] => ZPure[S1, S2, R, E, B] =
        _.map(f)
    }

  /**
   * The `IdentityBoth` instance for `State`.
   */
  implicit def StateIdentityBoth[S]: IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def any: State[S, Any] =
        State.unit
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        fa.zip(fb)
    }

  /**
   * The `IdentityFlatten` instance for `State`.
   */
  implicit def StateIdentityFlatten[S]: IdentityFlatten[({ type lambda[+A] = State[S, A] })#lambda] =
    new IdentityFlatten[({ type lambda[+A] = State[S, A] })#lambda] {
      def any: State[S, Any] =
        State.unit
      def flatten[A](ffa: State[S, State[S, A]]): State[S, A] =
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

  private final case class Succeed[S, +A](value: A) extends ZPure[S, S, Any, Nothing, A] {
    override def tag = Tags.Succeed
  }
  private final case class Fail[S, +E](error: E) extends ZPure[S, S, Any, E, Nothing] {
    override def tag = Tags.Fail
  }
  private final case class Modify[-S1, +S2, +E, +A](run: S1 => (S2, A)) extends ZPure[S1, S2, Any, E, A] {
    override def tag = Tags.Modify
  }
  private final case class FlatMap[-S1, S2, +S3, -R, +E, A, +B](
    value: ZPure[S1, S2, R, E, A],
    continue: A => ZPure[S2, S3, R, E, B]
  ) extends ZPure[S1, S3, R, E, B] {
    override def tag = Tags.FlatMap
  }
  private final case class Fold[-S1, S2, +S3, -R, E1, +E2, A, +B](
    value: ZPure[S1, S2, R, E1, A],
    failure: E1 => ZPure[S2, S3, R, E2, B],
    success: A => ZPure[S2, S3, R, E2, B]
  ) extends ZPure[S1, S3, R, E2, B]
      with Function[A, ZPure[S2, S3, R, E2, B]] {
    override def tag = Tags.Fold
    override def apply(a: A): ZPure[S2, S3, R, E2, B] =
      success(a)
  }
  private final case class Access[S1, S2, R, E, A](access: R => ZPure[R, S1, S2, E, A]) extends ZPure[S1, S2, R, E, A] {
    override def tag = Tags.Access
  }
  private final case class Provide[S1, S2, R, E, A](r: R, continue: ZPure[R, S1, S2, E, A])
      extends ZPure[S1, S2, Any, E, A] {
    override def tag = Tags.Provide
  }
}
