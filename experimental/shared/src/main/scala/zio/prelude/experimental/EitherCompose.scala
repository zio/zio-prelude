/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

package zio.prelude
package experimental

import zio._

trait EitherCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {

  type :+:[+_, +_]

  def toLeft[A]: A =>: (A :+: Nothing)
  def toRight[B]: B =>: (Nothing :+: B)
  def fromEither[A, B, C](a2c: => A =>: C)(b2c: => B =>: C): (A :+: B) =>: C

  def eitherCompose[A, B, C](
    a2c: A =>: C,
    b2c: B =>: C,
    ab2c: (A :+: B) =>: C
  )(implicit eqA2C: Equal[A =>: C], eqB2C: Equal[B =>: C], eqA2BC: Equal[(A :+: B) =>: C]): Boolean = {
    val law1 = compose[A, A :+: B, C](fromEither(a2c)(b2c), toLeft) === a2c
    val law2 = compose[B, A :+: B, C](fromEither(a2c)(b2c), toRight) === b2c
    val law3 = fromEither(compose[A, A :+: B, C](ab2c, toLeft))(compose[B, A :+: B, C](ab2c, toRight)) === ab2c

    law1 && law2 && law3
  }
}

object EitherCompose {

  type Aux[=>:[-_, +_], Sum[+_, +_]] = EitherCompose[=>:] {
    type :+:[+l, +r] = Sum[l, r]
  }

  implicit val FunctionEitherCompose: EitherCompose[Function] = new EitherCompose[Function] {

    type :+:[+l, +r] = Either[l, r]

    override def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      AssociativeCompose.FunctionIdentityCompose.compose(bc, ab)

    override def toLeft[A]: Function[A, Either[A, Nothing]] = Left(_)

    override def toRight[B]: Function[B, Either[Nothing, B]] = Right(_)

    override def fromEither[A, B, C](a2c: => Function[A, C])(b2c: => Function[B, C]): Function[Either[A, B], C] = {
      case Left(a)  => a2c(a)
      case Right(b) => b2c(b)
    }
  }

  implicit val URIOEitherCompose: EitherCompose[URIO] = new EitherCompose[URIO] {

    type :+:[+l, +r] = Either[l, r]

    def toLeft[A]: URIO[A, Either[A, Nothing]] = URIO.access(Left(_))

    def toRight[B]: URIO[B, Either[Nothing, B]] = URIO.access(Right(_))

    def fromEither[A, B, C](a2c: => URIO[A, C])(b2c: => URIO[B, C]): URIO[Either[A, B], C] =
      a2c ||| b2c

    def compose[A, B, C](bc: URIO[B, C], ab: URIO[A, B]): URIO[A, C] =
      AssociativeCompose.URIOIdentityCompose.compose(bc, ab)
  }

  implicit val URLayerEitherCompose: EitherCompose[URLayer] = new EitherCompose[URLayer] {

    type :+:[+l, +r] = Either[l, r]

    def toLeft[A]: URLayer[A, Either[A, Nothing]] = ZLayer.identity[A].map(Left(_))

    def toRight[B]: URLayer[B, Either[Nothing, B]] = ZLayer.identity[B].map(Right(_))

    def fromEither[A, B, C](a2c: => URLayer[A, C])(b2c: => URLayer[B, C]): URLayer[Either[A, B], C] = {
      val right: ZLayer[Either[A, B], A, B]                   =
        ZLayer.fromFunctionManyM[Either[A, B], A, B] {
          case Left(a)  => ZIO.fail(a)
          case Right(b) => ZIO.succeed(b)
        }
      val unright: URLayer[(Either[A, B], Cause[A]), A]       =
        ZLayer.fromFunctionManyM[(Either[A, B], Cause[A]), Nothing, A] { case (_, cause) =>
          cause.failureOrCause.fold(ZIO.succeed(_), ZIO.halt(_))
        }
      val handleFailure: URLayer[(Either[A, B], Cause[A]), C] = unright >>> a2c
      right.fold(handleFailure, b2c)
    }

    def compose[A, B, C](bc: URLayer[B, C], ab: URLayer[A, B]): URLayer[A, C] =
      AssociativeCompose.URLayerIdentityCompose.compose(bc, ab)
  }

  implicit val URManagedEitherCompose: EitherCompose[URManaged] = new EitherCompose[URManaged] {

    type :+:[+l, +r] = Either[l, r]

    def toLeft[A]: URManaged[A, Either[A, Nothing]] = ZManaged.access(Left(_))

    def toRight[B]: URManaged[B, Either[Nothing, B]] = ZManaged.access(Right(_))

    def fromEither[A, B, C](a2c: => URManaged[A, C])(b2c: => URManaged[B, C]): URManaged[Either[A, B], C] =
      a2c ||| b2c

    def compose[A, B, C](bc: URManaged[B, C], ab: URManaged[A, B]): URManaged[A, C] =
      AssociativeCompose.URManagedIdentityCompose.compose(bc, ab)
  }
}

trait EitherComposeSyntax {
  implicit class EitherComposeOps[A, C, =>:[-_, +_]](private val a2b: A =>: C) {

    /** A symbolic alias for `fromEither`. Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def |||[B, :+:[+_, +_]](implicit eitherCompose: EitherCompose.Aux[=>:, :+:]): (=> B =>: C) => ((A :+: B) =>: C) =
      eitherCompose.fromEither(a2b)

    /** Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def fromEither[B, :+:[+_, +_]](implicit
      eitherCompose: EitherCompose.Aux[=>:, :+:]
    ): (=> B =>: C) => ((A :+: B) =>: C) =
      eitherCompose.fromEither(a2b)
  }
}
