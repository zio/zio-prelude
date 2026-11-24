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
 * An experimental type class for an arrow type `=>:` with products: a product
 * type `:*:`, its two projections `fromFirst` and `fromSecond`, and `toBoth`,
 * which pairs the outputs of two arrows that read the same input. It extends
 * `AssociativeCompose`, which provides `compose`. It lives in the
 * experimental module, so its name and shape can change between releases.
 *
 * The built-in instance is `BothCompose.FunctionApplicationCompose`, for
 * `Function` with `Tuple2` as the product. Its type is
 * `ApplicationCompose[Function]`, so `:*:` is abstract from outside: a result
 * typed `B :*: C` is a `Tuple2` at runtime but is not known to be one by the
 * compiler.
 *
 * {{{
 * val compose = BothCompose.FunctionApplicationCompose
 * val length: String => Int   = _.length
 * val shout: String => String = _.toUpperCase
 * val both  = compose.toBoth(length)(shout)
 * val first = compose.compose(compose.fromFirst[Int], both)
 * val result: Int = first("zio")
 * // 3
 * }}}
 */
trait BothCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {

  /**
   * The product type of this arrow type.
   */
  type :*:[+_, +_]

  /**
   * The arrow that projects a product to its first component.
   */
  def fromFirst[A]: (A :*: Any) =>: A

  /**
   * The arrow that projects a product to its second component.
   */
  def fromSecond[B]: (Any :*: B) =>: B

  /**
   * Combines `a2b` and `a2c`, which read the same input, into one arrow that
   * returns the product of their outputs.
   */
  def toBoth[A, B, C](a2b: A =>: B)(a2c: A =>: C): A =>: (B :*: C)

  /**
   * Checks the product laws for the given arrows and returns `true` when all
   * three hold: `fromFirst` after `toBoth(a2b)(a2c)` equals `a2b`,
   * `fromSecond` after `toBoth(a2b)(a2c)` equals `a2c`, and `toBoth` of the
   * two projections of `a2bc` equals `a2bc`. Equality of arrows comes from the
   * implicit `Equal` instances.
   */
  def bothCompose[A, B, C](
    a2b: A =>: B,
    a2c: A =>: C,
    a2bc: A =>: (B :*: C)
  )(implicit eqA2B: Equal[A =>: B], eqA2C: Equal[A =>: C], eqA2BC: Equal[A =>: (B :*: C)]): Boolean = {
    val law1 = compose[A, B :*: C, B](fromFirst, toBoth(a2b)(a2c)) === a2b
    val law2 = compose[A, B :*: C, C](fromSecond, toBoth(a2b)(a2c)) === a2c
    val law3 = toBoth(compose[A, B :*: C, B](fromFirst, a2bc))(compose[A, B :*: C, C](fromSecond, a2bc)) === a2bc

    law1 && law2 && law3
  }
}

/**
 * Companion of [[BothCompose]], with the `Aux` alias and the `Function`
 * instance.
 */
object BothCompose {

  /**
   * A `BothCompose[=>:]` whose product type `:*:` is known to be `Product`.
   */
  type Aux[=>:[-_, +_], Product[+_, +_]] = BothCompose[=>:] {
    type :*:[+f, +s] = Product[f, s]
  }

  /**
   * The instance for `Function`, with `Tuple2` as the product and `Function`
   * as the exponential. It is the `BothCompose[Function]` and the
   * `ApplicationCompose[Function]` found by implicit search. Its declared type
   * is `ApplicationCompose[Function]`, so the product and exponential types
   * are abstract from outside, and implicit search does not find it as a
   * `BothCompose.Aux[Function, Tuple2]`.
   */
  implicit val FunctionApplicationCompose: ApplicationCompose[Function] = new ApplicationCompose[Function] {

    type :*:[+f, +s]  = Tuple2[f, s]
    type -->:[-t, +r] = Function[t, r]

    override def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      AssociativeCompose.FunctionIdentityCompose.compose(bc, ab)

    override def fromFirst[A]: Function[(A, Any), A] = _._1

    override def fromSecond[B]: Function[(Any, B), B] = _._2

    override def toBoth[A, B, C](a2b: Function[A, B])(a2c: Function[A, C]): Function[A, (B, C)] = { a =>
      (a2b(a), a2c(a))
    }

    override def application[A, B]: Function[(Function[A, B], A), B] = { case (a2b, a) =>
      a2b(a)
    }

    override def curry[A, B, C](f: Function[(A, B), C]): Function[A, Function[B, C]] = { a => b =>
      f((a, b))
    }

    override def uncurry[A, B, C](g: Function[A, Function[B, C]]): Function[(A, B), C] = { case (a, b) =>
      g(a)(b)
    }

  }

}

/**
 * Infix syntax for [[BothCompose]], mixed into the `zio.prelude.experimental`
 * package object.
 */
trait BothComposeSyntax {

  /**
   * Provides `&&&` and `toBoth` on an arrow `A =>: B`. They need an implicit
   * `BothCompose.Aux[=>:, :*:]`, which the built-in `Function` instance does
   * not provide.
   */
  implicit class BothComposeOps[A, B, =>:[-_, +_]](private val a2b: A =>: B) {

    /**
     * A symbolic alias for `toBoth`. Returns a function that takes an arrow
     * `A =>: C` and pairs it with this arrow into `A =>: (B :*: C)`.
     */
    def &&&[C, :*:[+_, +_]](implicit both: BothCompose.Aux[=>:, :*:]): (A =>: C) => (A =>: (B :*: C)) =
      both.toBoth(a2b)

    /**
     * Returns a function that takes an arrow `A =>: C` and pairs it with this
     * arrow into `A =>: (B :*: C)`, using the `toBoth` of the instance.
     */
    def toBoth[C, :*:[+_, +_]](implicit both: BothCompose.Aux[=>:, :*:]): (A =>: C) => (A =>: (B :*: C)) =
      both.toBoth(a2b)
  }
}
