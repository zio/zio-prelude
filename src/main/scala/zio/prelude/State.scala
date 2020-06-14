package zio.prelude

import scala.annotation.tailrec

import zio.prelude.State._

/**
 * `State[S, A]` is a purely functional description of a state transition
 * function that, given an initial state of type `S`, returns an updated state
 * of type `S` along with a value of type `A`. State can be used to model
 * computations that maintain state in a purely functional way, automatically
 * handling the bookkeeping of threading the state through the computation.
 */
sealed trait State[S, +A] { self =>

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[B](that: State[S, B]): State[S, B] =
    self zipRight that

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[B](that: State[S, B]): State[S, A] =
    self zipLeft that

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[B](that: State[S, B]): State[S, (A, B)] =
    self zip that

  final def flatMap[B](f: A => State[S, B]): State[S, B] =
    FlatMap(self, f)

  /**
   * Transforms the result of this state transition function with the
   * specified function.
   */
  final def map[B](f: A => B): State[S, B] =
    flatMap(a => State.succeed(f(a)))

  /**
   * Runs this state transition function with the specified initial state,
   * returning both the updated state and the result.
   */
  @tailrec
  final def run(s: S): (S, A) =
    self match {
      case Succeed(a)                    => (s, a)
      case Modify(f)                     => f(s)
      case FlatMap(Succeed(a), continue) => continue(a).run(s)
      case FlatMap(Modify(f), continue)  => f(s) match { case (s, a) => continue(a).run(s) }
      case FlatMap(FlatMap(x, f), g)     => x.flatMap(a => f(a).flatMap(g)).run(s)
    }

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both into a tuple.
   */
  final def zip[B](that: State[S, B]): State[S, (A, B)] =
    self.zipWith(that)((_, _))

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * this state transition function.
   */
  final def zipLeft[B](that: State[S, B]): State[S, A] =
    self.zipWith(that)((a, _) => a)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and returning the result of
   * that state transition function.
   */
  final def zipRight[B](that: State[S, B]): State[S, B] =
    self.zipWith(that)((_, b) => b)

  /**
   * Combines this state transition function with the specified state
   * transition function, passing the updated state from this state transition
   * function to that state transition function and combining the results of
   * both using the specified function.
   */
  final def zipWith[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))
}

object State {

  /**
   * Constructs a state transition function from a function that given an
   * initial state returns an updated state and a value.
   */
  def apply[S, A](run: S => (S, A)): State[S, A] =
    modify(run)

  /**
   * Constructs a state transition function that returns the state.
   */
  def get[S]: State[S, S] =
    modify(s => (s, s))

  /**
   * Constructs a state transition function from the specified modify
   * function.
   */
  def modify[S, A](f: S => (S, A)): State[S, A] =
    Modify(f)

  /**
   * Constructs a state transition function that sets the state to the
   * specified value.
   */
  def set[S](s: S): State[S, Unit] =
    modify(_ => (s, ()))

  /**
   * Constructs a state transition function that always returns the specified
   * value, passing the state through unchanged.
   */
  def succeed[S, A](a: => A): State[S, A] =
    modify(s => (s, a))

  /**
   * Lazily constructs a state transition function.
   */
  def suspend[S, A](state: => State[S, A]): State[S, A] =
    unit.flatMap(_ => state)

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
  def update[S](f: S => S): State[S, Unit] =
    modify(s => (f(s), ()))

  /**
   * The `AssociativeBoth` instance for `State`.
   */
  implicit def StateAssociativeBoth[S]: AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        State.suspend(fa.zip(fb))
    }

  /**
   * The `IdentityBoth` instance for `State`.
   */
  implicit def StateIdentityBoth[S]: IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def any: State[S, Any] =
        State.unit
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        State.suspend(fa.zip(fb))
    }

  /**
   * The `Covariant` instance for `State`.
   */
  implicit def StateCovariant[S]: Covariant[({ type lambda[+A] = State[S, A] })#lambda] =
    new Covariant[({ type lambda[+A] = State[S, A] })#lambda] {
      def map[A, B](f: A => B): State[S, A] => State[S, B] =
        _.map(f)
    }

  private final case class Succeed[S, A](value: A)                                          extends State[S, A]
  private final case class Modify[S, A](run: S => (S, A))                                   extends State[S, A]
  private final case class FlatMap[S, A, B](value: State[S, A], continue: A => State[S, B]) extends State[S, B]
}
