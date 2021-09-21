/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
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

/**
 * `FoldState` describes the different ways that a state transition function
 * `S1 => S2` that can be fail can be composed with an error handler `S3 => S4`
 * and a success handler `S5 => S6`. This allows defining `Unit => Unit` as an
 * identity for the success channel, allowing this to be used as the state type
 * for computations that just pass through the state unchanged. This
 * dramatically improves type inference and minimizes the number of type
 * parameters that need to be specified by users.
 */
sealed trait FoldState[S1, S2, S3, S4, S5, S6] {

  /**
   * The input type of the folded state transition function.
   */
  type In

  /**
   * The output type of the folded state transition function.
   */
  type Out

  def apply[E, E2](
    self: S1 => Either[E, S2],
    onFailure: S3 => Either[E2, S4],
    onSuccess: S5 => Either[E2, S6]
  ): In => Either[E2, Out]
}

object FoldState extends FoldStateImplicits {

  /**
   * The type of a `FoldState` instance including its input and output types.
   */
  type Out[S1, S2, S3, S4, S5, S6, In0, Out0] = FoldState[S1, S2, S3, S4, S5, S6] { type In = In0; type Out = Out0 }

  /**
   * Composes the identity function `Unit => Unit` and error and success state
   * transition functions `S1 => S2` into a state transition function
   * `S1 => S2` by running the left transition function and then running the
   * failure or success function based on whether it succeeded, discarding the
   * result of the left function.
   */
  final case class LeftIdentity[S1, S2]() extends FoldState[Unit, Unit, S1, S2, S1, S2] {
    type In  = S1
    type Out = S2
    def apply[E, E2](
      self: Unit => Either[E, Unit],
      onFailure: S1 => Either[E2, S2],
      onSuccess: S1 => Either[E2, S2]
    ): S1 => Either[E2, S2] =
      s1 => self(()).fold(_ => onFailure(s1), _ => onSuccess(s1))
  }

  /**
   * Composes a state transition function `S1 => S2` and the error function
   * `S1 => S2` into a state transition function `S1 => S2` by ignoring the
   * right success function.
   */
  final case class RightIdentity[S1, S2]() extends FoldState[S1, S2, S1, S2, Unit, Unit] {
    type In  = S1
    type Out = S2
    def apply[E, E2](
      self: S1 => Either[E, S2],
      onFailure: S1 => Either[E2, S2],
      onSuccess: Unit => Either[E2, Unit]
    ): S1 => Either[E2, S2] =
      s1 => self(s1).fold(_ => onFailure(s1), s2 => Right(s2))

  }

  /**
   * Composes the transition function `S1 => S2`, the error function
   * `S1 => S3`, and the success function `S2 => S3` into a state transition
   * function `S1 => S4`. This models a normal fold where two functions unify
   * the error and success values to a common result type.
   */
  final case class Compose[S1, S2, S3]() extends FoldState[S1, S2, S1, S3, S2, S3] {
    type In  = S1
    type Out = S3
    def apply[E, E2](
      self: S1 => Either[E, S2],
      onFailure: S1 => Either[E2, S3],
      onSuccess: S2 => Either[E2, S3]
    ): S1 => Either[E2, S3] =
      s1 => self(s1).fold(_ => onFailure(s1), s2 => onSuccess(s2))
  }

  /**
   * The `FoldState` instance when the state transition function is the
   * identity function `Unit => Unit`.
   */
  implicit def leftIdentity[S1, S2]: FoldState.Out[Unit, Unit, S1, S2, S1, S2, S1, S2] =
    FoldState.LeftIdentity()
}

trait FoldStateImplicits extends FoldStateLowPriorityImplicits {

  /**
   * The `FoldState` instance when the success function is the identity
   * function `Unit => Unit`.
   */
  implicit def rightIdentity[S1, S2]: FoldState.Out[S1, S2, S1, S2, Unit, Unit, S1, S2] =
    FoldState.RightIdentity()
}

trait FoldStateLowPriorityImplicits {

  /**
   * The `FoldState` instance when the error and success functions can unify
   * the original input and output state types to a common result type.
   */
  implicit def compose[S1, S2, S3]: FoldState.Out[S1, S2, S1, S3, S2, S3, S1, S3] =
    FoldState.Compose()
}
