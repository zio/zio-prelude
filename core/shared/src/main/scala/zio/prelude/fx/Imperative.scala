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

import scala.annotation.tailrec
import Imperative._

/**
 * An `Imperative[Dsl, E, A]` is a data structure that provides the ability to execute a user provided DSL as a sequence of operations.
 * From a theoretical standpoint `ImperativeDsl` is an implementation of a Free Monad.``
 */
sealed trait Imperative[Dsl[+_, +_], +E, +A] { self =>

  final def catchAll[E2, A1 >: A](
    f: E => Imperative[Dsl, E2, A1]
  ): Imperative[Dsl, E2, A1] = self match {
    case imp @ Sequence(dsl, onSuccess, onFailure) =>
      Sequence(
        dsl,
        (a: imp.InSuccess) => onSuccess(a).catchAll(f),
        (e: imp.InFailure) => onFailure(e).catchAll(f)
      )
    case _                                         =>
      Sequence[Dsl, E, E2, A, A1](self, Succeed(_), f)
  }

  final def flatMap[E1 >: E, B](f: A => Imperative[Dsl, E1, B]): Imperative[Dsl, E1, B] = self match {
    case imp @ Sequence(dsl, onSuccess, onFailure) =>
      Sequence(
        dsl,
        (a: imp.InSuccess) =>
          onSuccess(a)
            .flatMap(f),
        (e: imp.InFailure) => onFailure(e).flatMap(f)
      )
    case _                                         =>
      Sequence[Dsl, E, E1, A, B](self, f, Fail(_))
  }

  final def flatten[E1 >: E, B](implicit ev: A <:< Imperative[Dsl, E1, B]): Imperative[Dsl, E1, B] =
    self.flatMap(ev)

  def interpret[Executable[+_, +_]](
    interpreter: Imperative.Interpreter[Dsl, Executable]
  )(implicit exe: Imperative.ToExecutable[Executable]): Executable[E, A] = self match {
    case Imperative.Succeed(a)                                => exe.succeed(a)
    case Imperative.Fail(e)                                   => exe.fail(e)
    case Imperative.Eval(fa)                                  => interpreter.interpret(fa)
    case free @ Imperative.Sequence(fa, onSuccess, onFailure) =>
      exe.sequence(
        fa.interpret(interpreter),
        (a: free.InSuccess) => onSuccess(a).interpret(interpreter),
        (e: free.InFailure) => onFailure(e).interpret(interpreter)
      )
  }

  final def map[B](f: A => B): Imperative[Dsl, E, B] =
    self.flatMap(a => Imperative.Succeed(f(a)))

  final def mapError[E2](f: E => E2): Imperative[Dsl, E2, A] =
    self.catchAll(e => Imperative.Fail(f(e)))

  def unsafeInterpret(
    unsafeInterpreter: Imperative.UnsafeInterpreter[Dsl]
  ): Either[E, A] = {
    @tailrec
    def loop(
      free: Imperative[Dsl, Any, Any],
      stack: List[Imperative.Sequence[Dsl, Any, Any, Any, Any]]
    ): Either[E, A] =
      free match {
        case Imperative.Succeed(a)                =>
          stack match {
            case Imperative.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
            case Nil                                           => Right(a.asInstanceOf[A])
          }
        case Imperative.Fail(e)                   =>
          stack match {
            case Imperative.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
            case Nil                                           => Left(e.asInstanceOf[E])
          }
        case Imperative.Eval(fa)                  =>
          unsafeInterpreter.interpret(fa) match {
            case Left(e)  =>
              stack match {
                case Imperative.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
                case Nil                                           => Left(e.asInstanceOf[E])
              }
            case Right(a) =>
              stack match {
                case Imperative.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
                case Nil                                           => Right(a.asInstanceOf[A])
              }
          }
        case free @ Imperative.Sequence(fa, _, _) =>
          loop(fa, (free :: stack).asInstanceOf[List[Imperative.Sequence[Dsl, Any, Any, Any, Any]]])
      }
    loop(self, Nil)
  }
}

object Imperative {
  def eval[Dsl[+_, +_], E, A](fa: Dsl[E, A]): Imperative[Dsl, E, A] = Eval(fa)
  def fail[Dsl[+_, +_], E](e: E): Imperative[Dsl, E, Nothing]       = Fail(e)
  def succeed[Dsl[+_, +_], A](a: A): Imperative[Dsl, Nothing, A]    = Succeed(a)

  final case class Succeed[Dsl[+_, +_], A](a: A)          extends Imperative[Dsl, Nothing, A]
  final case class Fail[Dsl[+_, +_], E](a: E)             extends Imperative[Dsl, E, Nothing]
  final case class Eval[Dsl[+_, +_], E, A](fa: Dsl[E, A]) extends Imperative[Dsl, E, A]
  final case class Sequence[Dsl[+_, +_], E1, E2, A1, A2] private[Imperative] (
    dsl: Imperative[Dsl, E1, A1],
    onSuccess: A1 => Imperative[Dsl, E2, A2],
    onFailure: E1 => Imperative[Dsl, E2, A2]
  ) extends Imperative[Dsl, E2, A2] {
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
