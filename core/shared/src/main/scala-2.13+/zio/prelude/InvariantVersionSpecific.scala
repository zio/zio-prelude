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

import scala.collection.BuildFrom

trait InvariantVersionSpecific {

  /**
   * Derives a `ForEach[F]` from an `Iterable[F]`.
   */
  implicit def IterableForEach[F[+a] <: Iterable[a]](implicit derive: DeriveBuildFrom[F]): ForEach[F] =
    new ForEach[F] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        fa.foldLeft(derive.derive[B].newBuilder(fa).succeed)((bs, a) => bs.zipWith(f(a))(_ += _)).map(_.result())
    }

  trait DeriveBuildFrom[F[+_]] {
    def derive[A]: BuildFrom[F[Any], A, F[A]]
  }

  object DeriveBuildFrom {
    implicit def default[F[+_]](implicit bf: BuildFrom[F[Any], Any, F[Any]]): DeriveBuildFrom[F] =
      new DeriveBuildFrom[F] {
        def derive[A]: BuildFrom[F[Any], A, F[A]] =
          bf.asInstanceOf[BuildFrom[F[Any], A, F[A]]]
      }
  }
}
