/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

import zio.prelude.macros.Refined

package object newtypes {
  object Sum extends SubtypeF

  /**
   * A newtype representing addition.
   */
  type Sum[A] = Sum.Type[A]

  object Prod extends SubtypeF

  /**
   * A newtype representing multiplication.
   */
  type Prod[A] = Prod.Type[A]

  object Or extends Subtype[Boolean]

  /**
   * A newtype representing logical disjunction.
   */
  type Or = Or.Type

  object And extends Subtype[Boolean]

  /**
   * A newtype representing logical conjunction.
   */
  type And = And.Type

  object AndF extends SubtypeF

  /**
   * A newtype representing parameterized logical conjunction.
   */
  type AndF[+A] = AndF.Type[A]

  object OrF extends SubtypeF

  /**
   * A newtype representing parameterized logical disjunction.
   */
  type OrF[+A] = OrF.Type[A]

  object First extends SubtypeF

  /**
   * A newtype representing taking the first of two elements.
   */
  type First[A] = First.Type[A]

  object Last extends SubtypeF

  /**
   * A newtype representing taking the last of two elements.
   */
  type Last[A] = Last.Type[A]

  object Min extends SubtypeF

  /**
   * A newtype representing taking the min of two elements.
   */
  type Min[A] = Min.Type[A]

  object Max extends SubtypeF {}

  /**
   * A newtype representing taking the max of two elements.
   */
  type Max[A] = Max.Type[A]

  /**
   * A newtype representing another type in a failed state
   */
  object Failure extends NewtypeF

  type Failure[+A] = Failure.Type[A]

  /**
   * A newtype representing an input error in another type
   */
  object FailureIn extends NewtypeF

  type FailureIn[+A] = FailureIn.Type[A]

  /**
   * A newtype representing an output error in another type
   */
  object FailureOut extends NewtypeF

  type FailureOut[+A] = FailureOut.Type[A]

  val NaturalType = Refined[Int](zio.prelude.refined.Assertion.greaterThan(-1))
  object Natural extends NaturalType.Subtype {
    type Type <: Int

    val one: Natural =
      Natural(1)

    val zero: Natural =
      Natural(0)

    def successor(n: Natural): Natural =
      Natural.unsafeApply(n + 1)

    def times(x: Natural, y: Natural): Natural = {
      val product = x * y
      if (x == 0 || product / x != y) Natural.unsafeApply(Int.MaxValue) else Natural.unsafeApply(product)
    }

    def plus(x: Natural, y: Natural): Natural = {
      val sum = x + y
      if (sum < 0) Natural.unsafeApply(Int.MaxValue) else Natural.unsafeApply(sum)
    }

    def minus(x: Natural, y: Natural): Natural = {
      val difference = x - y
      if (difference < 0) zero else Natural.unsafeApply(difference)
    }

    private[prelude] def unsafeMake(n: Int): Natural =
      Natural.unsafeApply(n)
  }

  type Natural = Natural.Type
}
