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

import scala.annotation.implicitNotFound

/**
 * `IdentityFlatten` describes a type that can be "flattened" in an
 * associative way and has an identity element with respect to that operation.
 * For example, with a list we can always vacuously add a layer by wrapping a
 * list in another list constructor and flattening the resulting list always
 * returns the original list unchanged.
 *
 * @see [[https://zio.dev/zio-prelude/functional-abstractions/parameterized-types/identityflatten/ IdentityFlatten]]
 */
@implicitNotFound("No implicit IdentityFlatten defined for ${F}.")
trait IdentityFlatten[F[+_]] extends AssociativeFlatten[F] { self =>

  /**
   * The identity element, such as `List(())` for `List` or `Some(())` for
   * `Option`. Mapping each value of an `F[A]` to `any` with that value inside
   * and flattening the result returns the original `F[A]`.
   */
  def any: F[Any]
}

/**
 * Summoner for `IdentityFlatten`. The instances for standard library and ZIO
 * types are in the `AssociativeFlatten` companion object.
 */
object IdentityFlatten {

  /**
   * Summons an implicit `IdentityFlatten[F]`.
   */
  def apply[F[+_]](implicit identityFlatten: IdentityFlatten[F]): IdentityFlatten[F] =
    identityFlatten

}

/**
 * Syntax for `IdentityFlatten`, available with `import zio.prelude._`.
 */
trait IdentityFlattenSyntax {

  /**
   * Provides infix syntax for identity operations for covariant types.
   */
  implicit class IdentityFlattenCovariantOps[F[+_], A](fa: F[A]) {

    /**
     * Returns `any` with its values replaced by `None` when `b` is `true`, and
     * this value with its values wrapped in `Some` otherwise.
     *
     * {{{
     * val skipped: Option[Option[Int]] = Option(1).unless(true)
     * // Some(None)
     * }}}
     */
    def unless(b: => Boolean)(implicit identity: IdentityFlatten[F], covariant: Covariant[F]): F[Option[A]] =
      if (b) identity.any.as(None) else fa.map(Some(_))

    /**
     * Returns this value with its values wrapped in `Some` when `b` is `true`,
     * and `any` with its values replaced by `None` otherwise.
     *
     * {{{
     * val kept: Option[Option[Int]] = Option(1).when(true)
     * // Some(Some(1))
     * val skipped: List[Option[Int]] = List(1, 2).when(false)
     * // List(None)
     * }}}
     */
    def when(b: => Boolean)(implicit identity: IdentityFlatten[F], covariant: Covariant[F]): F[Option[A]] =
      if (b) fa.map(Some(_)) else identity.any.as(None)
  }
}
