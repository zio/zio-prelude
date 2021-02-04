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

  /**
   * A newtype representing logical disjunction.
   */
  type Or = OrF[Boolean]
  object Or {
    def apply(boolean: Boolean): Or = OrF(boolean)
  }

  /**
   * A newtype representing logical conjunction.
   */
  type And = AndF[Boolean]
  object And {
    def apply(boolean: Boolean): And = AndF(boolean)
  }

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

  object Max extends SubtypeF

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

//  implicit def OrFBoolToOr(orFBool: OrF[Boolean]): Or      = Or(orFBool: Boolean)
//  implicit def AndFBoolToAnd(andFBool: AndF[Boolean]): And = And(andFBool: Boolean)
}
