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

trait BothCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {

  type :*:[+_, +_]

  def fromFirst[A]: (A :*: Any) =>: A
  def fromSecond[B]: (Any :*: B) =>: B
  def toBoth[A, B, C](a2b: A =>: B)(a2c: A =>: C): A =>: (B :*: C)

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

object BothCompose {

  type Aux[=>:[-_, +_], Product[+_, +_]] = BothCompose[=>:] {
    type :*:[+f, +s] = Product[f, s]
  }

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

  implicit val URIOApplicationCompose: BothCompose[URIO] = new BothCompose[URIO] {

    type :*:[+f, +s] = Tuple2[f, s]

    def fromFirst[A]: URIO[(A, Any), A] = URIO.access[(A, Any)](_._1)

    def fromSecond[B]: URIO[(Any, B), B] = URIO.access[(Any, B)](_._2)

    def toBoth[A, B, C](a2b: URIO[A, B])(a2c: URIO[A, C]): URIO[A, (B, C)] =
      a2b &&& a2c

    def compose[A, B, C](bc: URIO[B, C], ab: URIO[A, B]): URIO[A, C] =
      AssociativeCompose.URIOIdentityCompose.compose(bc, ab)
  }

  implicit val URLayerApplicationCompose: BothCompose[URLayer] = new BothCompose[URLayer] {

    type :*:[+f, +s] = Tuple2[f, s]

    def fromFirst[A]: URLayer[(A, Any), A] = ZLayer.first

    def fromSecond[B]: URLayer[(Any, B), B] = ZLayer.second

    def toBoth[A, B, C](a2b: URLayer[A, B])(a2c: URLayer[A, C]): URLayer[A, (B, C)] =
      a2b <&> a2c

    def compose[A, B, C](bc: URLayer[B, C], ab: URLayer[A, B]): URLayer[A, C] =
      AssociativeCompose.URLayerIdentityCompose.compose(bc, ab)
  }

  implicit val URManagedApplicationCompose: BothCompose[URManaged] = new BothCompose[URManaged] {

    type :*:[+f, +s] = Tuple2[f, s]

    def fromFirst[A]: URManaged[(A, Any), A] = ZManaged.first

    def fromSecond[B]: URManaged[(Any, B), B] = ZManaged.second

    def toBoth[A, B, C](a2b: URManaged[A, B])(a2c: URManaged[A, C]): URManaged[A, (B, C)] =
      a2b &&& a2c

    def compose[A, B, C](bc: URManaged[B, C], ab: URManaged[A, B]): URManaged[A, C] =
      AssociativeCompose.URManagedIdentityCompose.compose(bc, ab)
  }

}

trait BothComposeSyntax {
  implicit class BothComposeOps[A, B, =>:[-_, +_]](private val a2b: A =>: B) {

    /** A symbolic alias for `toBoth`. Composes `A -> B` with `A -> C` to form `A -> (B, C)`. */
    def &&&[C, :*:[+_, +_]](implicit both: BothCompose.Aux[=>:, :*:]): (A =>: C) => (A =>: (B :*: C)) =
      both.toBoth(a2b)

    /** Composes `A -> B` with `A -> C` to form `A -> (B, C)`. */
    def toBoth[C, :*:[+_, +_]](implicit both: BothCompose.Aux[=>:, :*:]): (A =>: C) => (A =>: (B :*: C)) =
      both.toBoth(a2b)
  }
}
