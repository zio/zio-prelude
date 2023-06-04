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

import com.github.ghik.silencer.silent

import scala.annotation.implicitNotFound

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[(A, B)]` with an identity.
 */
@implicitNotFound("No implicit IdentityBoth defined for ${F}.")
trait IdentityBoth[F[_]] extends AssociativeBoth[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[(A, B)]`.
   */
  def any: F[Any]
}

@silent("Unused import")
object IdentityBoth {
  import zio._ // for zio.EitherCompat

  /**
   * Summons an implicit `IdentityBoth[F]`.
   */
  def apply[F[_]](implicit identityBoth: IdentityBoth[F]): IdentityBoth[F] =
    identityBoth

  /**
   * The `IdentityBoth` instance for `Const`.
   */
  implicit def ConstIdentityBoth[A: Identity]: IdentityBoth[({ type ConstA[+B] = Const[A, B] })#ConstA] =
    new IdentityBoth[({ type ConstA[+B] = Const[A, B] })#ConstA] {
      val any: Const[A, Any]                                                   = Const(Identity[A].identity)
      def both[B, C](fb: => Const[A, B], fc: => Const[A, C]): Const[A, (B, C)] =
        Const.wrap(Const.unwrap(fb) <> Const.unwrap(fc))
    }

  def fromCovariantIdentityFlatten[F[+_]](implicit
    covariant: Covariant[F],
    identityFlatten: IdentityFlatten[F]
  ): IdentityBoth[F] = new IdentityBoth[F] {

    override def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = fa.map(a => fb.map(b => (a, b))).flatten

    override def any: F[Any] = identityFlatten.any

  }
}

trait IdentityBothSyntax {

  implicit class IdentityBothAnyOps[A](a: => A) {
    def succeed[F[+_]](implicit both: IdentityBoth[F], covariant: Covariant[F]): F[A] =
      both.any.map(_ => a)
  }

}
