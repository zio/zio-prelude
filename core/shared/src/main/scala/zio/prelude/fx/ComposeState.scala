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
 * `ComposeState` describes the different ways that a state transition function
 * `S1 => S2` and a state transition function `S3 => S4` can be composed. This
 * allows defining the function `Unit => Unit` as an identity for composing
 * state transition functions, allowing this to be used as the state type for
 * computations that just pass through the state unchanged. This dramatically
 * improves type inference and minimizes the number of type parameters that
 * need to be specified by users.
 */
sealed trait ComposeState[S1, S2, S3, S4] {

  /**
   * The input type of the composed state transition function.
   */
  type In

  /**
   * The output type of the composed state transition function.
   */
  type Out

  /**
   * Composed a state transition function `S1 => S2` and a state transition
   * function `S3 => S4` into a state transition function `In => Out`.
   */
  def compose(left: S1 => S2, right: S3 => S4): In => Out
}

object ComposeState extends ComposeStateImplicits {

  /**
   * The type of a `ComposeState` instance including its input and output
   *   types.
   */
  type Out[S1, S2, S3, S4, In0, Out0] = ComposeState[S1, S2, S3, S4] { type In = In0; type Out = Out0 }

  /**
   * Summons an implicit `ComposeState` instance.
   */
  def apply[S1, S2, S3, S4](implicit ev: ComposeState[S1, S2, S3, S4]): Out[S1, S2, S3, S4, ev.In, ev.Out] = ev

  /**
   * Composes the identity function `Unit => Unit` and a state transition
   * function `S1 => S2` into a state transition function `S1 => S2`  by simply
   * ignoring the left function and returning the right function unchanged.
   */
  final case class LeftIdentity[S1, S2]() extends ComposeState[Unit, Unit, S1, S2] {
    type In  = S1
    type Out = S2
    def compose(left: Unit => Unit, right: S1 => S2): In => Out =
      right
  }

  /**
   * Composes a state transition function `S1 => S2` and the identity function
   * `Unit => Unit` into a state transition function `S1 => S2` by ignoring the
   * right function and returning the left function unchanged.
   */
  final case class RightIdentity[S1, S2, S3]() extends ComposeState[S1, S2, Unit, Unit] {
    type In  = S1
    type Out = S2
    def compose(left: S1 => S2, right: Unit => Unit): In => Out =
      left
  }

  /**
   * Composes the transition function `S1 => S2` and the state transition
   * function `S3 => S4` into a state transition function `S1 => S4` when the
   * output type of the first function is a subtype of the input type of the
   * second function. This models normal function composition of state
   * transition functions.
   */
  final case class Compose[S1, S2 <: S3, S3, S4]() extends ComposeState[S1, S2, S3, S4] {
    type In  = S1
    type Out = S4
    def compose(left: S1 => S2, right: S3 => S4): In => Out =
      right compose left
  }

  /**
   * The `ComposeState` instance when the left state transition function is the
   * identity function `Unit => Unit`.
   */
  implicit def leftIdentity[S1, S2]: Out[Unit, Unit, S1, S2, S1, S2] =
    ComposeState.LeftIdentity()
}

trait ComposeStateImplicits extends ComposeStateLowPriorityImplicits {

  /**
   * The `ComposeState` instance when the right state transition function is
   * the identity function `Unit => Unit`.
   */
  implicit def rightIdentity[S1, S2]: ComposeState.Out[S1, S2, Unit, Unit, S1, S2] =
    ComposeState.RightIdentity()
}

trait ComposeStateLowPriorityImplicits {

  /**
   * The `ComposeState` instance when the output type of the left state
   * transition function is a subtype of the input type of the right state
   * transition function.
   */
  implicit def compose[S1, S2 <: S3, S3, S4]: ComposeState.Out[S1, S2, S3, S4, S1, S4] =
    ComposeState.Compose()
}
