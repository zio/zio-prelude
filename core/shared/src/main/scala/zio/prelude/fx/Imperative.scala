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
import zio.prelude.EReader
import scala.annotation.tailrec
import Imperative._

/**
 * An `Imperative[Dsl, E, A]` is a data structure that provides the ability to execute a user provided DSL as a sequence of operations.
 * From a theoretical standpoint `ImperativeDsl` is an implementation of a Free Monad.``
 */
sealed trait Imperative[Dsl[-_, +_, +_], -R, +E, +A] { self =>

  final def catchAll[R1 <: R, E2, A1 >: A](
    f: E => Imperative[Dsl, R1, E2, A1]
  ): Imperative[Dsl, R1, E2, A1] = self match {
    case imp @ Sequence(dsl, onSuccess, onFailure) =>
      Sequence(
        dsl,
        (a: imp.InSuccess) => onSuccess(a).catchAll(f),
        (e: imp.InFailure) => onFailure(e).catchAll(f)
      )
    case _                                         =>
      Sequence[Dsl, R1, E, E2, A, A1](self, Succeed(_), f)
  }

  final def flatMap[R1 <: R, E1 >: E, B](f: A => Imperative[Dsl, R1, E1, B]): Imperative[Dsl, R1, E1, B] = self match {
    case imp @ Sequence(dsl, onSuccess, onFailure) =>
      Sequence(
        dsl,
        (a: imp.InSuccess) =>
          onSuccess(a)
            .flatMap(f),
        (e: imp.InFailure) => onFailure(e).flatMap(f)
      )
    case _                                         =>
      Sequence[Dsl, R1, E, E1, A, B](self, f, Fail(_))
  }

  final def flatten[R1 <: R, E1 >: E, B](implicit ev: A <:< Imperative[Dsl, R1, E1, B]): Imperative[Dsl, R1, E1, B] =
    self.flatMap(ev)

  def interpret[Executable[-_, +_, +_]](
    interpreter: Imperative.Interpreter[Dsl, Executable]
  )(implicit exe: Imperative.ToExecutable[Executable]): Executable[R, E, A] = self match {
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

  final def map[B](f: A => B): Imperative[Dsl, R, E, B] =
    self.flatMap(a => Imperative.Succeed(f(a)))

  final def mapError[E2](f: E => E2): Imperative[Dsl, R, E2, A] =
    self.catchAll(e => Imperative.Fail(f(e)))

  def unsafeInterpret(
    unsafeInterpreter: Imperative.UnsafeInterpreter[Dsl]
  ): Either[E, A] = {
    @tailrec
    def loop(
      free: Imperative[Dsl, R, Any, Any],
      stack: List[Imperative.Sequence[Dsl, R, Any, Any, Any, Any]]
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
          loop(fa, (free :: stack).asInstanceOf[List[Imperative.Sequence[Dsl, R, Any, Any, Any, Any]]])
      }
    loop(self, Nil)
  }
}

object Imperative {
  def eval[Dsl[-_, +_, +_], R, E, A](dsl: Dsl[R, E, A]): Imperative[Dsl, R, E, A] = Eval(dsl)
  def fail[Dsl[-_, +_, +_], E](error: E): Imperative[Dsl, Any, E, Nothing]        = Fail(error)
  def succeed[Dsl[-_, +_, +_], A](value: A): Imperative[Dsl, Any, Nothing, A]     = Succeed(value)

  final case class Succeed[Dsl[-_, +_, +_], A](a: A)                 extends Imperative[Dsl, Any, Nothing, A]
  final case class Fail[Dsl[-_, +_, +_], E](error: E)                extends Imperative[Dsl, Any, E, Nothing]
  final case class Eval[Dsl[-_, +_, +_], R, E, A](dsl: Dsl[R, E, A]) extends Imperative[Dsl, R, E, A]
  final case class Sequence[Dsl[-_, +_, +_], R, E1, E2, A1, A2] private[Imperative] (
    dsl: Imperative[Dsl, R, E1, A1],
    onSuccess: A1 => Imperative[Dsl, R, E2, A2],
    onFailure: E1 => Imperative[Dsl, R, E2, A2]
  ) extends Imperative[Dsl, R, E2, A2] {
    type InSuccess = A1
    type InFailure = E1
  }

  /// Interpreter provides the ability to interpret a DSL into an executable program
  trait Interpreter[Dsl[-_, +_, +_], Executable[-_, +_, +_]] { self =>
    def interpret[R, E, A](dsl: Dsl[R, E, A]): Executable[R, E, A]

//    def combine[R2, Dsl2[-_, +_, +_]](
//      that: Interpreter[Dsl2, Executable]
//    ): Interpreter[({ type lambda[-R, +E, +A] = CompositeDsl[Dsl, Dsl2, R, R2, E, A] })#lambda, Executable] =
//      new Interpreter[({ type lambda[-R, +E, +A] = CompositeDsl[Dsl, Dsl2, R, R2, E, A] })#lambda, Executable] {
//        override def interpret[R, E, A](dsl: CompositeDsl[Dsl, Dsl2, R, R2, E, A]): Executable[R with R2, E, A] =
//          dsl.eitherDsl match {
//            case Left(dsl)  => self.interpret(dsl)
//            case Right(dsl) => that.interpret(dsl)
//          }
//      }
  }

  trait UnsafeInterpreter[Dsl[-_, +_, +_]] {
    def interpret[R, E, A](dsl: Dsl[R, E, A]): Either[E, A]
  }

  trait ToExecutable[Executable[-_, +_, +_]] {
    def succeed[A](a: A): Executable[Any, Nothing, A]
    def fail[E](e: E): Executable[Any, E, Nothing]
    def eval[R, E, A](fa: Executable[R, E, A]): Executable[R, E, A]
    def sequence[R, E1, E2, A1, A2](
      fa: Executable[R, E1, A1],
      onSuccess: A1 => Executable[R, E2, A2],
      onFailure: E1 => Executable[R, E2, A2]
    ): Executable[R, E2, A2]
  }

//  final case class CompositeDsl[+Dsl1[-_, +_, +_], +Dsl2[-_, +_, +_], -R1, -R2, +E, +A](
//    eitherDsl: Either[Dsl1[R1, E, A], Dsl2[R2, E, A]]
//  ) extends AnyVal { self =>
//    type InSuccess <: A
//    type InFailure <: E
//  }

  implicit def ZPureToExecutable: ToExecutable[EReader] =
    new ToExecutable[EReader] {

      override def succeed[A](a: A): EReader[Any, Nothing, A] = EReader.succeed(a)

      override def fail[E](e: E): EReader[Any, E, Nothing] = EReader.fail(e)

      override def eval[R, E, A](fa: EReader[R, E, A]): EReader[R, E, A] = EReader.suspend(fa)

      override def sequence[R, E1, E2, A1, A2](
        fa: EReader[R, E1, A1],
        onSuccess: A1 => EReader[R, E2, A2],
        onFailure: E1 => EReader[R, E2, A2]
      ): EReader[R, E2, A2] = EReader.suspend {
        fa.foldM(
          onFailure,
          onSuccess
        )
      }
    }
}
