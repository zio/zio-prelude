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

/**
 * An `AssociativeCompose` that also has an identity value of type `A =>: A`
 * for every `A`: composing it with any value on either side returns that
 * value.
 */
trait IdentityCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {

  /**
   * The identity value of type `A =>: A`, such as the identity function.
   */
  def identity[A]: A =>: A

  /**
   * Returns whether composing `ab` with `identity` on either side returns a
   * value equal to `ab`, which the identity law requires.
   */
  def identityCompose[A, B](
    ab: A =>: B
  )(implicit eq: Equal[A =>: B]): Boolean = {
    val ab1 = compose(identity[B], ab)
    val ab2 = compose(ab, identity[A])

    eq.equal(ab1, ab) && eq.equal(ab2, ab)
  }
}
