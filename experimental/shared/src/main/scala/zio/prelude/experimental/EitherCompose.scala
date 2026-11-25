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

package zio.prelude
package experimental

/**
 * An experimental type class for an arrow type `=>:` with sums, the dual of
 * [[BothCompose]]: a sum type `:+:`, its two injections `toLeft` and
 * `toRight`, and `fromEither`, which combines two arrows with the same output
 * type into one arrow from the sum. It extends `AssociativeCompose`, which
 * provides `compose`. It lives in the experimental module, so its name and
 * shape can change between releases.
 *
 * The built-in instance is `EitherCompose.FunctionEitherCompose`, for
 * `Function` with `Either` as the sum. Its type is `EitherCompose[Function]`,
 * so `:+:` is abstract from outside.
 *
 * {{{
 * val compose = EitherCompose.FunctionEitherCompose
 * val fromInt: Int => String      = n => "int " + n
 * val fromBool: Boolean => String = b => "bool " + b
 * val describe = compose.fromEither(fromInt)(fromBool)
 * val viaLeft  = compose.compose(describe, compose.toLeft[Int])
 * val result: String = viaLeft(3)
 * // int 3
 * }}}
 */
trait EitherCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {

  /**
   * The sum type of this arrow type.
   */
  type :+:[+_, +_]

  /**
   * The arrow that injects a value into the left side of the sum.
   */
  def toLeft[A]: A =>: (A :+: Nothing)

  /**
   * The arrow that injects a value into the right side of the sum.
   */
  def toRight[B]: B =>: (Nothing :+: B)

  /**
   * Combines `a2c` and `b2c`, which return the same type, into one arrow from
   * the sum `A :+: B`: it applies `a2c` to a left value and `b2c` to a right
   * value.
   */
  def fromEither[A, B, C](a2c: => A =>: C)(b2c: => B =>: C): (A :+: B) =>: C

  /**
   * Checks the sum laws for the given arrows and returns `true` when all three
   * hold: `fromEither(a2c)(b2c)` after `toLeft` equals `a2c`, the same after
   * `toRight` equals `b2c`, and `fromEither` of `ab2c` after each injection
   * equals `ab2c`. Equality of arrows comes from the implicit `Equal`
   * instances.
   */
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

/**
 * Companion of [[EitherCompose]], with the `Aux` alias and the `Function`
 * instance.
 */
object EitherCompose {

  /**
   * An `EitherCompose[=>:]` whose sum type `:+:` is known to be `Sum`.
   */
  type Aux[=>:[-_, +_], Sum[+_, +_]] = EitherCompose[=>:] {
    type :+:[+l, +r] = Sum[l, r]
  }

  /**
   * The instance for `Function`, with `Either` as the sum. Its declared type
   * is `EitherCompose[Function]`, so the sum type is abstract from outside,
   * and implicit search does not find it as an
   * `EitherCompose.Aux[Function, Either]`.
   */
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

}

/**
 * Infix syntax for [[EitherCompose]], mixed into the `zio.prelude.experimental`
 * package object.
 */
trait EitherComposeSyntax {

  /**
   * Provides `|||` and `fromEither` on an arrow `A =>: C`. They need an
   * implicit `EitherCompose.Aux[=>:, :+:]`, which the built-in `Function`
   * instance does not provide.
   */
  implicit class EitherComposeOps[A, C, =>:[-_, +_]](private val a2b: A =>: C) {

    /**
     * A symbolic alias for `fromEither`. Returns a function that takes an
     * arrow `B =>: C` and combines it with this arrow into `(A :+: B) =>: C`.
     */
    def |||[B, :+:[+_, +_]](implicit eitherCompose: EitherCompose.Aux[=>:, :+:]): (=> B =>: C) => ((A :+: B) =>: C) =
      eitherCompose.fromEither(a2b)

    /**
     * Returns a function that takes an arrow `B =>: C` and combines it with
     * this arrow into `(A :+: B) =>: C`, using the `fromEither` of the
     * instance.
     */
    def fromEither[B, :+:[+_, +_]](implicit
      eitherCompose: EitherCompose.Aux[=>:, :+:]
    ): (=> B =>: C) => ((A :+: B) =>: C) =
      eitherCompose.fromEither(a2b)
  }
}
