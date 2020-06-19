package zio.prelude

import scala.annotation.tailrec

import zio.prelude.ZState._

/**
 * `ZState[S1, S2, A]` is a purely functional description of a state transition
 * function that, given an initial state of type `S1`, returns an updated state
 * of type `S2` along with a value of type `A`. State can be used to model
 * computations that maintain state in a purely functional way, automatically
 * handling the bookkeeping of threading the state through the computation.
 */
sealed trait ZState[-S1, +S2, +A] { self =>

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, B] =
    self zipRight that

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, A] =
    self zipLeft that

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, (A, B)] =
    self zip that

  /**
   * Transforms the initial state of this state transition function with the
   * specified function.
   */
  final def contramap[S0](f: S0 => S1): ZState[S0, S2, A] =
    update(f) *> self

  /**
   * Extends this state transition function with another state transition
   * function that depends on the result of this state transition function by
   * running the first state transition function, using its result to generate
   * a second state transition function, and running that state transition
   * function with the updated state.
   */
  final def flatMap[S3, B](f: A => ZState[S2, S3, B]): ZState[S1, S3, B] =
    FlatMap(self, f)

  /**
   * Flattens a nested state transition function to a single state transition
   * function by running the outer state transition function and then running
   * the inner state transition function with the updated state.
   */
  final def flatten[S3, B](implicit ev: A <:< ZState[S2, S3, B]): ZState[S1, S3, B] =
    flatMap(ev)

  /**
   * Transforms the result of this state transition function with the
   * specified function.
   */
  final def map[B](f: A => B): ZState[S1, S2, B] =
    flatMap(a => ZState.succeed(f(a)))

  /**
   * Transforms the updated state of this state transition function with the
   * specified function.
   */
  final def mapState[S3](f: S2 => S3): ZState[S1, S3, A] =
    self <* update(f)

  /**
   * Runs this state transition function with the specified initial state,
   * returning both the updated state and the result.
   */
  final def run(s: S1): (S2, A) = {

    @tailrec
    def loop(self: ZState[Any, Any, Any])(s: Any): (Any, Any) =
      self match {
        case Succeed(value)                    => (s, value)
        case Modify(f)                         => f(s)
        case FlatMap(Succeed(value), continue) => loop(continue(value))(s)
        case FlatMap(Modify(f), continue)      => f(s) match { case (s, a) => loop(continue(a))(s) }
        case FlatMap(FlatMap(x, f), g)         => loop(x.flatMap(a => f(a).flatMap(g)))(s)
      }

    loop(self.asInstanceOf[ZState[Any, Any, Any]])(s).asInstanceOf[(S2, A)]
  }

  /**
   * Runs this state transition function with the specified initial state,
   * returning the result and discarding the updated state.
   */
  final def runResult(s: S1): A =
    run(s)._2

  /**
   * Runs this state transition function with the specified initial state,
   * returning the updated state and discarding the result.
   */
  final def runState(s: S1): S2 =
    run(s)._1

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both into a tuple.
   */
  final def zip[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, (A, B)] =
    self.zipWith(that)((_, _))

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * this state transition function.
   */
  final def zipLeft[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, A] =
    self.zipWith(that)((a, _) => a)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * that state transition function.
   */
  final def zipRight[S3, B](that: ZState[S2, S3, B]): ZState[S1, S3, B] =
    self.zipWith(that)((_, b) => b)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both using the specified function.
   */
  final def zipWith[S3, B, C](that: ZState[S2, S3, B])(f: (A, B) => C): ZState[S1, S3, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))
}

object ZState {

  /**
   * Constructs a state transition function from a function that given an
   * initial state returns an updated state and a value.
   */
  def apply[S1, S2, A](run: S1 => (S2, A)): ZState[S1, S2, A] =
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
  def modify[S1, S2, A](f: S1 => (S2, A)): ZState[S1, S2, A] =
    Modify(f)

  /**
   * Constructs a state transition function that sets the state to the
   * specified value.
   */
  def set[S](s: S): ZState[Any, S, Unit] =
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
  def update[S1, S2](f: S1 => S2): ZState[S1, S2, Unit] =
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
  implicit def StateContravariant[S2, A]: Contravariant[({ type lambda[-S1] = ZState[S1, S2, A] })#lambda] =
    new Contravariant[({ type lambda[-S1] = ZState[S1, S2, A] })#lambda] {
      def contramap[S1, S0](f: S0 => S1): ZState[S1, S2, A] => ZState[S0, S2, A] =
        _.contramap(f)
    }

  /**
   * The `Covariant` instance for `State`.
   */
  implicit def StateCovariant[S1, S2]: Covariant[({ type lambda[+A] = ZState[S1, S2, A] })#lambda] =
    new Covariant[({ type lambda[+A] = ZState[S1, S2, A] })#lambda] {
      def map[A, B](f: A => B): ZState[S1, S2, A] => ZState[S1, S2, B] =
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

  private final case class Succeed[S, +A](value: A)                 extends ZState[S, S, A]
  private final case class Modify[-S1, +S2, +A](run: S1 => (S2, A)) extends ZState[S1, S2, A]
  private final case class FlatMap[-S1, S2, +S3, A, +B](value: ZState[S1, S2, A], continue: A => ZState[S2, S3, B])
      extends ZState[S1, S3, B]
}
