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

import zio.Chunk

/**
 * A `SafeFunction` is a function that can be freely composed with the
 * guarantee that functions of arbitrary size can be evaluated in constant
 * stack space. It does this by maintaining each of the composed functions
 * internally in a data structure and evaluating them in a loop when the
 * function is called.
 */
final class SafeFunction[-In, +Out] private (private val steps: Chunk[Any => Any]) extends Function1[In, Out] { self =>

  /**
   * Evaluates the function with the specified argument.
   */
  def apply(in: In): Out = {
    val iterator = steps.iterator
    var out: Any = in
    while (iterator.hasNext) {
      val f = iterator.next()
      out = f(out)
    }
    out.asInstanceOf[Out]
  }

  /**
   * Composes this function with the specified function to return a new
   * function that feeds the output of this function into the specified
   * function.
   */
  def andThen[Out2](that: SafeFunction[Out, Out2]): SafeFunction[In, Out2] =
    new SafeFunction(self.steps ++ that.steps)

  /**
   * Composes this function with the specified function to return a new
   * function that feeds the output of the specified function into this
   * function.
   */
  def compose[In2](that: SafeFunction[In2, In]): SafeFunction[In2, Out] =
    new SafeFunction(that.steps ++ self.steps)
}

object SafeFunction {

  /**
   * Constructs a safe function from an ordinary Scala function.
   */
  def apply[In, Out](f: In => Out): SafeFunction[In, Out] =
    new SafeFunction(Chunk(f.asInstanceOf[Any => Any]))
}
