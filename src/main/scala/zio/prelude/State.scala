package zio.prelude

import scala.annotation.tailrec

import zio.prelude.State._

sealed trait State[S, +A] { self =>

  def <*>[B](that: State[S, B]): State[S, (A, B)] =
    self zip that

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    FlatMap(self, f)

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.succeed(f(a)))

  @tailrec
  final def run(s: S): (S, A) =
    self match {
      case Succeed(a)                    => (s, a)
      case Modify(f)                     => f(s)
      case FlatMap(Succeed(a), continue) => continue(a).run(s)
      case FlatMap(Modify(f), continue)  =>
        val (s1, a) = f(s)
        continue(a).run(s1)
      case FlatMap(FlatMap(x, f), g)     =>
        x.flatMap(a => f(a).flatMap(g)).run(s)
    }

  def zip[B](that: State[S, B]): State[S, (A, B)] =
    zipWith(that)((_, _))

  def zipWith[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    self.flatMap(a => that.flatMap(b => State.succeed(f(a, b))))
}

object State {

  private final case class Succeed[S, A](value: A)                                          extends State[S, A]
  private final case class Modify[S, A](run: S => (S, A))                                   extends State[S, A]
  private final case class FlatMap[S, A, B](value: State[S, A], continue: A => State[S, B]) extends State[S, B]

  def apply[S, A](run: S => (S, A)): State[S, A] =
    Modify(run)

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => (s, ()))

  def succeed[S, A](a: A): State[S, A] =
    State(s => (s, a))

  def unit[S]: State[S, Unit] =
    State(s => (s, ()))

  implicit def StateAssociativeBoth[S]: AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        fa.zip(fb)
    }
  implicit def StateIdentityBoth[S]: IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] {
      def any: State[S, Any] =
        State.unit
      def both[A, B](fa: => State[S, A], fb: => State[S, B]): State[S, (A, B)] =
        fa.zip(fb)
    }
  implicit def StateCovariant[S]: Covariant[({ type lambda[+A] = State[S, A] })#lambda] =
    new Covariant[({ type lambda[+A] = State[S, A] })#lambda] {
      def map[A, B](f: A => B): State[S, A] => State[S, B] =
        _.map(f)
    }
}
