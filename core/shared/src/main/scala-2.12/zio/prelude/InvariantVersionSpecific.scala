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

import zio.prelude.coherent.CovariantIdentityBoth

import scala.collection.generic.CanBuildFrom

trait InvariantVersionSpecific {

  /**
   * Derives a `ForEach[F]` from an `Iterable[F]`.
   */
  implicit def IterableForEach[F[+a] <: Iterable[a]](implicit derive: DeriveCanBuildFrom[F]): ForEach[F] =
    new ForEach[F] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]          =
        CovariantIdentityBoth[G].forEach(fa)(f)(derive.derive)
      override def collectM[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(
        f: A => G[Option[B]]
      )(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): G[F[B]] =
        CovariantIdentityBoth[G].collectM(fa)(f)(derive.derive)
      override def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(fa)(f)
    }

  trait DeriveCanBuildFrom[F[+_]] {
    def derive[A]: CanBuildFrom[F[Any], A, F[A]]
  }

  object DeriveCanBuildFrom {
    implicit def default[F[+_]](implicit bf: CanBuildFrom[F[Any], Any, F[Any]]): DeriveCanBuildFrom[F] =
      new DeriveCanBuildFrom[F] {
        def derive[A]: CanBuildFrom[F[Any], A, F[A]] =
          bf.asInstanceOf[CanBuildFrom[F[Any], A, F[A]]]
      }
  }
}
