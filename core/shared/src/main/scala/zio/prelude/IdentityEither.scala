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
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[Either[A, B]]` with an identity value.
 *
 * @see [[https://zio.dev/zio-prelude/functional-abstractions/parameterized-types/identityeither/ IdentityEither]]
 */
@implicitNotFound("No implicit IdentityEither defined for ${F}.")
trait IdentityEither[F[_]] extends AssociativeEither[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[Either[A, B]]`, such as `None` for `Option` or the empty
   * list for `List`.
   */
  def none: F[Nothing]
}

/**
 * Summoner for `IdentityEither`. The instances for standard library and ZIO
 * types are in the `AssociativeEither` companion object.
 */
object IdentityEither {

  /**
   * Summons an implicit `IdentityEither[F]`.
   */
  def apply[F[_]](implicit identityEither: IdentityEither[F]): IdentityEither[F] =
    identityEither
}

/**
 * Syntax for `IdentityEither`, available with `import zio.prelude._`.
 */
trait IdentityEitherSyntax {

  /**
   * Provides syntax to get the identity of an `IdentityEither` from any value.
   */
  implicit class IdentityEitherAnyOps(a: Any) {

    /**
     * Ignores its argument and returns a "failed" `F`: `IdentityEither[F].none`,
     * such as `None` for `Option`.
     */
    def fail[F[_]](implicit either: IdentityEither[F]): F[Nothing] =
      either.none
  }

}
