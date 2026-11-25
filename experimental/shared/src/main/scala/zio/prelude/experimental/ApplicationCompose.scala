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
 * An experimental type class that extends [[BothCompose]] with an exponential
 * type `-->:`, which represents arrows as values, so that arrows over a
 * product can be curried and uncurried. Together with `BothCompose` it
 * describes a cartesian closed category. It lives in the experimental module,
 * so its name and shape can change between releases.
 *
 * The built-in instance is `BothCompose.FunctionApplicationCompose`, for
 * `Function` with `Tuple2` as the product and `Function` as the exponential.
 * Its type is `ApplicationCompose[Function]`, so `:*:` and `-->:` are
 * abstract from outside.
 */
trait ApplicationCompose[=>:[-_, +_]] extends BothCompose[=>:] {

  /**
   * The exponential type of this arrow type: an arrow from `A` to `B` as a
   * value.
   */
  type -->:[-_, +_]

  /**
   * The arrow that applies an arrow value to an input, both given as a
   * product.
   */
  def application[A, B]: ((A -->: B) :*: A) =>: B

  /**
   * Turns an arrow from the product `A :*: B` into an arrow from `A` that
   * returns an arrow value from `B`.
   */
  def curry[A, B, C](f: (A :*: B) =>: C): A =>: (B -->: C)

  /**
   * Turns an arrow from `A` that returns an arrow value from `B` into an arrow
   * from the product `A :*: B`. It is the inverse of `curry`.
   */
  def uncurry[A, B, C](g: A =>: (B -->: C)): (A :*: B) =>: C

  /**
   * Checks the exponential laws for the given arrows and returns `true` when
   * all three hold: `uncurry(curry(f))` equals `f`, `curry(uncurry(g))`
   * equals `g`, and `application` after pairing `curry(f)` (applied to the
   * first component) with the second component equals `f`. Equality of
   * arrows comes from the implicit `Equal` instances.
   */
  def applicationCompose[A, B, C](
    f: (A :*: B) =>: C,
    g: A =>: (B -->: C)
  )(implicit eqF: Equal[(A :*: B) =>: C], eqG: Equal[A =>: (B -->: C)]): Boolean = {
    val law1 = uncurry(curry(f)) === f
    val law2 = curry(uncurry(g)) === g
    val law3 = compose[A :*: B, (B -->: C) :*: B, C](
      application[B, C],
      toBoth(compose[A :*: B, A, B -->: C](curry(f), fromFirst))(fromSecond)
    ) === f

    law1 && law2 && law3
  }
}

/**
 * Companion of [[ApplicationCompose]], with the `Aux` alias. The `Function`
 * instance lives in the `BothCompose` companion.
 */
object ApplicationCompose {

  /**
   * An `ApplicationCompose[=>:]` whose product type `:*:` is known to be
   * `Product` and whose exponential type `-->:` is known to be `Arrow`.
   */
  type Aux[=>:[-_, +_], Product[+_, +_], Arrow[-_, +_]] = ApplicationCompose[=>:] {
    type :*:[+f, +s]  = Product[f, s]
    type -->:[-t, +r] = Arrow[t, r]
  }
}

/**
 * Infix syntax for [[ApplicationCompose]], mixed into the
 * `zio.prelude.experimental` package object.
 */
trait ApplicationComposeSyntax {

  /**
   * Provides `curry` on an arrow from a product. It needs an implicit
   * `ApplicationCompose.Aux[=>:, :*:, -->:]`, which the built-in `Function`
   * instance does not provide.
   */
  implicit class ApplicationComposeCurryOps[A, B, C, =>:[-_, +_], :*:[+_, +_], -->:[-_, +_]](
    private val ab2c: (A :*: B) =>: C
  ) {

    /**
     * Curries this arrow from `A :*: B` into an arrow from `A` that returns an
     * arrow value `B -->: C`, using the `curry` of the instance.
     */
    def curry(implicit applicationCompose: ApplicationCompose.Aux[=>:, :*:, -->:]): A =>: (B -->: C) =
      applicationCompose.curry(ab2c)
  }

  /**
   * Provides `uncurry` on an arrow that returns an arrow value. It needs an
   * implicit `ApplicationCompose.Aux[=>:, :*:, -->:]`, which the built-in
   * `Function` instance does not provide.
   */
  implicit class ApplicationComposeUncurryOps[A, B, C, =>:[-_, +_], :*:[+_, +_], -->:[-_, +_]](
    private val a2b2c: A =>: (B -->: C)
  ) {

    /**
     * Uncurries this arrow `A =>: (B -->: C)` into an arrow from `A :*: B`,
     * using the `uncurry` of the instance.
     */
    def uncurry(implicit applicationCompose: ApplicationCompose.Aux[=>:, :*:, -->:]): (A :*: B) =>: C =
      applicationCompose.uncurry(a2b2c)
  }
}
