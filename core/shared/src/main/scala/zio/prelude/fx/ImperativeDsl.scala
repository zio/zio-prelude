package zio.prelude.fx

import scala.annotation.tailrec
import ImperativeDsl._

sealed trait ImperativeDsl[Op[+_, +_], +E, +A] { self =>

  final def catchAll[E2, A1 >: A](
    f: E => ImperativeDsl[Op, E2, A1]
  ): ImperativeDsl[Op, E2, A1] = self match {
    case free @ Sequence(fa, onSuccess, onOpailure) =>
      Sequence(
        fa,
        (a: free.InSuccess) => onSuccess(a).catchAll(f),
        (e: free.InOpailure) => onOpailure(e).catchAll(f)
      )
    case _                                          => ImperativeDsl.Sequence[Op, E, E2, A, A1](self, ImperativeDsl.Succeed(_), f)
  }

  final def flatMap[E1 >: E, B](f: A => ImperativeDsl[Op, E1, B]): ImperativeDsl[Op, E1, B] = self match {
    case free @ Sequence(fa, onSuccess, onOpailure) =>
      Sequence(fa, (a: free.InSuccess) => onSuccess(a).flatMap(f), (e: free.InOpailure) => onOpailure(e).flatMap(f))
    case _                                          => ImperativeDsl.Sequence[Op, E, E1, A, B](self, f, ImperativeDsl.Opail(_))
  }

  final def flatten[E1 >: E, B](implicit ev: A <:< ImperativeDsl[Op, E1, B]): ImperativeDsl[Op, E1, B] =
    self.flatMap(ev)

  def interpret[G[+_, +_]](
    interpreter: ImperativeDsl.Interpreter[Op, G]
  )(implicit g: ImperativeDsl.Executable[G]): G[E, A] = self match {
    case ImperativeDsl.Succeed(a)                                 => g.succeed(a)
    case ImperativeDsl.Opail(e)                                   => g.fail(e)
    case ImperativeDsl.Eval(fa)                                   => interpreter.interpret(fa)
    case free @ ImperativeDsl.Sequence(fa, onSuccess, onOpailure) =>
      g.sequence(
        fa.interpret(interpreter),
        (a: free.InSuccess) => onSuccess(a).interpret(interpreter),
        (e: free.InOpailure) => onOpailure(e).interpret(interpreter)
      )
  }

  final def map[B](f: A => B): ImperativeDsl[Op, E, B] =
    self.flatMap(a => ImperativeDsl.Succeed(f(a)))

  final def mapError[E2](f: E => E2): ImperativeDsl[Op, E2, A] =
    self.catchAll(e => ImperativeDsl.Opail(f(e)))

  def unsafeInterpret(
    unsafeInterpreter: ImperativeDsl.UnsafeInterpreter[Op]
  ): Either[E, A] = {
    @tailrec
    def loop(
      free: ImperativeDsl[Op, Any, Any],
      stack: List[ImperativeDsl.Sequence[Op, Any, Any, Any, Any]]
    ): Either[E, A] =
      free match {
        case ImperativeDsl.Succeed(a)                                 =>
          stack match {
            case ImperativeDsl.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
            case Nil                                              => Right(a.asInstanceOf[A])
          }
        case ImperativeDsl.Opail(e)                                   =>
          stack match {
            case ImperativeDsl.Sequence(_, _, onOpailure) :: stack => loop(onOpailure(e), stack)
            case Nil                                               => Left(e.asInstanceOf[E])
          }
        case ImperativeDsl.Eval(fa)                                   =>
          unsafeInterpreter.interpret(fa) match {
            case Left(e)  =>
              stack match {
                case ImperativeDsl.Sequence(_, _, onOpailure) :: stack => loop(onOpailure(e), stack)
                case Nil                                               => Left(e.asInstanceOf[E])
              }
            case Right(a) =>
              stack match {
                case ImperativeDsl.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
                case Nil                                              => Right(a.asInstanceOf[A])
              }
          }
        case free @ ImperativeDsl.Sequence(fa, onSuccess, onOpailure) =>
          loop(fa, (free :: stack).asInstanceOf[List[ImperativeDsl.Sequence[Op, Any, Any, Any, Any]]])
      }
    loop(self, Nil)
  }
}

object ImperativeDsl {
  def eval[Op[+_, +_], E, A](fa: Op[E, A]): ImperativeDsl[Op, E, A] = Eval(fa)
  def fail[Op[+_, +_], E](e: E): ImperativeDsl[Op, E, Nothing]      = Opail(e)
  def succeed[Op[+_, +_], A](a: A): ImperativeDsl[Op, Nothing, A]   = Succeed(a)

  final case class Succeed[Op[+_, +_], A](a: A)         extends ImperativeDsl[Op, Nothing, A]
  final case class Opail[Op[+_, +_], E](a: E)           extends ImperativeDsl[Op, E, Nothing]
  final case class Eval[Op[+_, +_], E, A](fa: Op[E, A]) extends ImperativeDsl[Op, E, A]
  final case class Sequence[Op[+_, +_], E1, E2, A1, A2] private[ImperativeDsl] (
    fa: ImperativeDsl[Op, E1, A1],
    onSuccess: A1 => ImperativeDsl[Op, E2, A2],
    onOpailure: E1 => ImperativeDsl[Op, E2, A2]
  ) extends ImperativeDsl[Op, E2, A2] {
    type InSuccess  = A1
    type InOpailure = E1
  }

  // TODO: Consider renaming G to Dsl/Executable
  trait Interpreter[Op[+_, +_], G[+_, +_]] {
    def interpret[E, A](fa: Op[E, A]): G[E, A]

    def combine[Op2[+_, +_]](
      that: Interpreter[Op2, G]
    ): Interpreter[({ type lambda[+E, +A] = CompositeOp[Op, Op2, E, A] })#lambda, G] = ???
  }

  trait UnsafeInterpreter[Op[+_, +_]] {
    def interpret[E, A](fa: Op[E, A]): Either[E, A]
  }

  trait Executable[F[+_, +_]] {
    def succeed[A](a: A): F[Nothing, A]
    def fail[E](e: E): F[E, Nothing]
    def eval[E, A](fa: F[E, A]): F[E, A]
    def sequence[E1, E2, A1, A2](
      fa: F[E1, A1],
      onSuccess: A1 => F[E2, A2],
      onFailure: E1 => F[E2, A2]
    ): F[E2, A2]
  }

  // TODO: Consider Making: CompositeOp[+Op1[+_, +_], +Op2[+_, +_], +E, +A]
  final case class CompositeOp[Op1[+_, +_], Op2[+_, +_], +E, +A](run: Either[Op1[E, A], Op2[E, A]]) { self => }

  // TODO: Consider what can be done to make the type lambda here simpler
  implicit def ZPureExecutable[W]: Executable[({ type lambda[+E, +A] = ZPure[W, Unit, Unit, Any, E, A] })#lambda] = {
    // ({ type lambda[+E, +A] = ZPure[W, Unit, Unit, Any, E, A] })#lambda
    type Result[+E, +A] = ZPure[W, Unit, Unit, Any, E, A]
    val Result: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure
    new Executable[Result] {

      override def succeed[A](a: A): Result[Nothing, A] = Result.succeed(a)

      override def fail[E](e: E): Result[E, Nothing] = Result.fail(e)

      override def eval[E, A](fa: Result[E, A]): Result[E, A] = Result.suspend(fa)

      override def sequence[E1, E2, A1, A2](
        fa: Result[E1, A1],
        onSuccess: A1 => Result[E2, A2],
        onFailure: E1 => Result[E2, A2]
      ): Result[E2, A2] = Result.suspend {
        // TODO: Consider if this can be done with foldM since its pissible E1 or E2 is Nothing
        fa.foldM(
          onFailure,
          onSuccess
        )
      }
    }
  }
}
