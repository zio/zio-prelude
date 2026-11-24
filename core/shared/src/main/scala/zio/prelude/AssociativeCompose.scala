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

import zio.ZLayer.ZLayerProvideSomeOps
import zio._

/**
 * `AssociativeCompose[=>:]` describes how to compose values of a type
 * `A =>: B` that behave like functions from `A` to `B`, such as `Function1`
 * or `URLayer`. Composition must be associative: composing `ab`, `bc` and
 * `cd` gives the same result whichever pair is composed first.
 */
trait AssociativeCompose[=>:[-_, +_]] {

  /**
   * Composes `ab` with `bc`: the result goes from `A` to `C` through `B`.
   *
   * {{{
   * val compose = AssociativeCompose.FunctionIdentityCompose
   *
   * val plusOneThenDouble: Int => Int = compose.compose((_: Int) * 2, (_: Int) + 1)
   * val result: Int = plusOneThenDouble(3)
   * // 8
   * }}}
   */
  def compose[A, B, C](bc: B =>: C, ab: A =>: B): A =>: C

  /**
   * Returns whether composing `ab`, `bc` and `cd` in either grouping gives
   * equal results, which the associativity law requires.
   */
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

/**
 * Instances of `AssociativeCompose` and `IdentityCompose`.
 */
object AssociativeCompose {

  /**
   * The `IdentityCompose` instance for functions: `compose` is function
   * composition and `identity` is the identity function.
   */
  implicit val FunctionIdentityCompose: IdentityCompose[Function] = new IdentityCompose[Function] {

    def identity[A]: A => A = scala.Predef.identity

    def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      bc.compose(ab)

  }

  /**
   * The `IdentityCompose` instance for effects of type
   * `URIO[A, ZEnvironment[B]]`. Composing runs the first effect and provides
   * its result as the environment of the second, and `identity` is
   * `ZIO.environment`.
   */
  implicit val URIOIdentityCompose: IdentityCompose[({ type lambda[-r, +a] = URIO[r, ZEnvironment[a]] })#lambda] =
    new IdentityCompose[({ type lambda[-r, +a] = URIO[r, ZEnvironment[a]] })#lambda] {
      def identity[A]: URIO[A, ZEnvironment[A]] = ZIO.environment

      def compose[A, B, C](bc: URIO[B, ZEnvironment[C]], ab: URIO[A, ZEnvironment[B]]): URIO[A, ZEnvironment[C]] =
        ab.flatMap(b => bc.provideEnvironment(b))
    }

  /**
   * The `IdentityCompose` instance for `URLayer`. Composing feeds the output
   * of the first layer to the second with `>>>`, and `identity` is
   * `ZLayer.environment`.
   */
  implicit val URLayerIdentityCompose: IdentityCompose[URLayer] = new IdentityCompose[URLayer] {
    def identity[A]: URLayer[A, A] = ZLayer.environment

    def compose[A, B, C](bc: URLayer[B, C], ab: URLayer[A, B]): URLayer[A, C] =
      new ZLayerProvideSomeOps[A, Nothing, B](ab) >>> bc
  }
}

/**
 * Syntax for `AssociativeCompose`, available with `import zio.prelude._`.
 */
trait AssociativeComposeSyntax {

  /**
   * Provides syntax to compose a value of type `A =>: B` with another one.
   *
   * Each method takes only the implicit `AssociativeCompose` and returns a
   * function that takes the other value, so `ab >>> bc` does not compile:
   * `bc` is taken as the implicit argument. Call the returned function
   * instead, as in `ab.>>>[C].apply(bc)`. A type's own method with the same
   * name, such as `Function1#andThen`, takes precedence.
   */
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
