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

import zio._

trait AssociativeCompose[=>:[-_, +_]] {
  def compose[A, B, C](bc: B =>: C, ab: A =>: B): A =>: C

  def associativeCompose[A, B, C, D](
    ab: A =>: B,
    bc: B =>: C,
    cd: C =>: D
  )(implicit eq: Equal[A =>: D]): Boolean = {
    val ad1 = compose(cd, compose(bc, ab))
    val ad2 = compose(compose(cd, bc), ab)

    eq.equal(ad1, ad2)
  }
}

object AssociativeCompose {

  implicit val FunctionIdentityCompose: IdentityCompose[Function] = new IdentityCompose[Function] {

    def identity[A]: A => A = scala.Predef.identity

    def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      bc.compose(ab)

  }

  implicit val URIOIdentityCompose: IdentityCompose[URIO] = new IdentityCompose[URIO] {
    def identity[A]: URIO[A, A] = URIO.environment

    def compose[A, B, C](bc: URIO[B, C], ab: URIO[A, B]): URIO[A, C] = ab >>> bc
  }

  implicit val URLayerIdentityCompose: IdentityCompose[URLayer] = new IdentityCompose[URLayer] {
    def identity[A]: URLayer[A, A] = ZLayer.identity

    def compose[A, B, C](bc: URLayer[B, C], ab: URLayer[A, B]): URLayer[A, C] = ab >>> bc
  }

  implicit val URManagedIdentityCompose: IdentityCompose[URManaged] = new IdentityCompose[URManaged] {
    def identity[A]: URManaged[A, A] = ZManaged.identity

    def compose[A, B, C](bc: URManaged[B, C], ab: URManaged[A, B]): URManaged[A, C] = ab >>> bc
  }
}

trait AssociativeComposeSyntax {
  implicit class AssociativeComposeOps[A, B, =>:[-_, +_]](private val ab: A =>: B) {

    /** A symbolic alias for `andThen`. Composes `A -> B` with `B -> C` to form `A -> C`. */
    def >>>[C](implicit ev: AssociativeCompose[=>:]): (B =>: C) => (A =>: C) = { bc =>
      ev.compose(bc, ab)
    }

    /** Composes `A -> B` with `B -> C` to form `A -> C`. */
    def andThen[C](implicit ev: AssociativeCompose[=>:]): (B =>: C) => (A =>: C) = { bc =>
      ev.compose(bc, ab)
    }

    /** A symbolic alias for `compose`. Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def <<<[Z](implicit ev: AssociativeCompose[=>:]): (Z =>: A) => Z =>: B = { za =>
      ev.compose(ab, za)
    }

    /** Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def compose[Z](implicit ev: AssociativeCompose[=>:]): (Z =>: A) => Z =>: B = { za =>
      ev.compose(ab, za)
    }
  }
}
