package zio.prelude.fx

import scala.annotation.tailrec
import ImperativeDsl._

/**
 * An `ImperativeDsl[Dsl, E, A]` is a data structure that provides the ability to execute a user provided DSL as a sequence of operations.
 * From a theoretical standpoint `ImperativeDsl` is an implementation of a Free Monad.``
 * @tparam Dsl - the user's DSL
 * @tparam E - the error type if any
 * @tparam A - the result type
 */
sealed trait ImperativeDsl[Dsl[+_, +_], +E, +A] { self =>

  final def catchAll[E2, A1 >: A](
    f: E => ImperativeDsl[Dsl, E2, A1]
  ): ImperativeDsl[Dsl, E2, A1] = self match {
    case free @ Sequence(fa, onSuccess, onFailure) =>
      Sequence(
        fa,
        (a: free.InSuccess) => onSuccess(a).catchAll(f),
        (e: free.InFailure) => onFailure(e).catchAll(f)
      )
    case _                                         => ImperativeDsl.Sequence[Dsl, E, E2, A, A1](self, ImperativeDsl.Succeed(_), f)
  }

  final def flatMap[E1 >: E, B](f: A => ImperativeDsl[Dsl, E1, B]): ImperativeDsl[Dsl, E1, B] = self match {
    case free @ Sequence(fa, onSuccess, onFailure) =>
      Sequence(
        fa,
        (a: free.InSuccess) =>
          onSuccess(a)
            .flatMap(f),
        (e: free.InFailure) => onFailure(e).flatMap(f)
      )
    case _                                         => ImperativeDsl.Sequence[Dsl, E, E1, A, B](self, f, ImperativeDsl.Opail(_))
  }

  final def flatten[E1 >: E, B](implicit ev: A <:< ImperativeDsl[Dsl, E1, B]): ImperativeDsl[Dsl, E1, B] =
    self.flatMap(ev)

  def interpret[Executable[+_, +_]](
    interpreter: ImperativeDsl.Interpreter[Dsl, Executable]
  )(implicit exe: ImperativeDsl.ToExecutable[Executable]): Executable[E, A] = self match {
    case ImperativeDsl.Succeed(a)                                => exe.succeed(a)
    case ImperativeDsl.Opail(e)                                  => exe.fail(e)
    case ImperativeDsl.Eval(fa)                                  => interpreter.interpret(fa)
    case free @ ImperativeDsl.Sequence(fa, onSuccess, onFailure) =>
      exe.sequence(
        fa.interpret(interpreter),
        (a: free.InSuccess) => onSuccess(a).interpret(interpreter),
        (e: free.InFailure) => onFailure(e).interpret(interpreter)
      )
  }

  final def map[B](f: A => B): ImperativeDsl[Dsl, E, B] =
    self.flatMap(a => ImperativeDsl.Succeed(f(a)))

  final def mapError[E2](f: E => E2): ImperativeDsl[Dsl, E2, A] =
    self.catchAll(e => ImperativeDsl.Opail(f(e)))

  def unsafeInterpret(
    unsafeInterpreter: ImperativeDsl.UnsafeInterpreter[Dsl]
  ): Either[E, A] = {
    @tailrec
    def loop(
      free: ImperativeDsl[Dsl, Any, Any],
      stack: List[ImperativeDsl.Sequence[Dsl, Any, Any, Any, Any]]
    ): Either[E, A] =
      free match {
        case ImperativeDsl.Succeed(a)                =>
          stack match {
            case ImperativeDsl.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
            case Nil                                              => Right(a.asInstanceOf[A])
          }
        case ImperativeDsl.Opail(e)                  =>
          stack match {
            case ImperativeDsl.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
            case Nil                                              => Left(e.asInstanceOf[E])
          }
        case ImperativeDsl.Eval(fa)                  =>
          unsafeInterpreter.interpret(fa) match {
            case Left(e)  =>
              stack match {
                case ImperativeDsl.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
                case Nil                                              => Left(e.asInstanceOf[E])
              }
            case Right(a) =>
              stack match {
                case ImperativeDsl.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
                case Nil                                              => Right(a.asInstanceOf[A])
              }
          }
        case free @ ImperativeDsl.Sequence(fa, _, _) =>
          loop(fa, (free :: stack).asInstanceOf[List[ImperativeDsl.Sequence[Dsl, Any, Any, Any, Any]]])
      }
    loop(self, Nil)
  }
}

object ImperativeDsl {
  def eval[Op[+_, +_], E, A](fa: Op[E, A]): ImperativeDsl[Op, E, A] = Eval(fa)
  def fail[Op[+_, +_], E](e: E): ImperativeDsl[Op, E, Nothing]      = Opail(e)
  def succeed[Op[+_, +_], A](a: A): ImperativeDsl[Op, Nothing, A]   = Succeed(a)

  final case class Succeed[Op[+_, +_], A](a: A)         extends ImperativeDsl[Op, Nothing, A]
  final case class Fail[Op[+_, +_], E](a: E)           extends ImperativeDsl[Op, E, Nothing]
  final case class Eval[Op[+_, +_], E, A](fa: Op[E, A]) extends ImperativeDsl[Op, E, A]
  final case class Sequence[Op[+_, +_], E1, E2, A1, A2] private[ImperativeDsl] (
    fa: ImperativeDsl[Op, E1, A1],
    onSuccess: A1 => ImperativeDsl[Op, E2, A2],
    onFailure: E1 => ImperativeDsl[Op, E2, A2]
  ) extends ImperativeDsl[Op, E2, A2] {
    type InSuccess = A1
    type InFailure = E1
  }

  /// Interpreter provides the ability to interpret a DSL into an executable program
  trait Interpreter[Dsl[+_, +_], Executable[+_, +_]] { self =>
    def interpret[E, A](dsl: Dsl[E, A]): Executable[E, A]

    def combine[Dsl2[+_, +_]](
      that: Interpreter[Dsl2, Executable]
    ): Interpreter[({ type lambda[+E, +A] = CompositeDsl[Dsl, Dsl2, E, A] })#lambda, Executable] =
      new Interpreter[({ type lambda[+E, +A] = CompositeDsl[Dsl, Dsl2, E, A] })#lambda, Executable] {
        override def interpret[E, A](dsl: CompositeDsl[Dsl, Dsl2, E, A]): Executable[E, A] = dsl.eitherDsl match {
          case Left(dsl)  => self.interpret(dsl)
          case Right(dsl) => that.interpret(dsl)
        }
      }
  }

  trait UnsafeInterpreter[Dsl[+_, +_]] {
    def interpret[E, A](fa: Dsl[E, A]): Either[E, A]
  }

  trait ToExecutable[Executable[+_, +_]] {
    def succeed[A](a: A): Executable[Nothing, A]
    def fail[E](e: E): Executable[E, Nothing]
    def eval[E, A](fa: Executable[E, A]): Executable[E, A]
    def sequence[E1, E2, A1, A2](
      fa: Executable[E1, A1],
      onSuccess: A1 => Executable[E2, A2],
      onFailure: E1 => Executable[E2, A2]
    ): Executable[E2, A2]
  }

  final case class CompositeDsl[+Dsl1[+_, +_], +Dsl2[+_, +_], +E, +A](eitherDsl: Either[Dsl1[E, A], Dsl2[E, A]])
      extends AnyVal { self =>
    type InSuccess <: A
    type InFailure <: E
  }

  // TODO: Consider what can be done to make the type lambda here simpler
  implicit def ZPureToExecutable[W]
    : ToExecutable[({ type lambda[+E, +A] = ZPure[W, Unit, Unit, Any, E, A] })#lambda] = {
    // ({ type lambda[+E, +A] = ZPure[W, Unit, Unit, Any, E, A] })#lambda
    type Result[+E, +A] = ZPure[W, Unit, Unit, Any, E, A]
    val Result: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure
    new ToExecutable[Result] {

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
