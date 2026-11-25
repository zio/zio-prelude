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

/**
 * Members of the `Invariant` companion object that differ between Scala
 * versions: a `ForEach` instance for collection types that extend `Iterable`,
 * built on the Scala 2.12 `CanBuildFrom` or, on Scala 2.13 and 3, on
 * `BuildFrom`.
 */
trait InvariantVersionSpecific {

  /**
   * Derives a `ForEach[F]` for a collection type `F` that extends `Iterable`,
   * such as `Seq` or `Queue`, from the collection's builder factory.
   */
  implicit def IterableForEach[F[+a] <: Iterable[a]](implicit derive: DeriveCanBuildFrom[F]): ForEach[F] =
    new ForEach[F] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]          =
        CovariantIdentityBoth[G].forEach(fa)(f)(derive.derive)
      override def collectM[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(
        f: A => G[Option[B]]
      )(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): G[F[B]]       =
        CovariantIdentityBoth[G].collectM(fa)(f)(derive.derive)
      override def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
        CovariantIdentityBoth[G].forEach_(fa)(f)
    }

  /**
   * Provides a `CanBuildFrom` that builds an `F[A]` for every element type `A`.
   */
  trait DeriveCanBuildFrom[F[+_]] {

    /**
     * Returns the `CanBuildFrom` that builds an `F[A]` from `A` values.
     */
    def derive[A]: CanBuildFrom[F[Any], A, F[A]]
  }

  /**
   * Instances of `DeriveCanBuildFrom`.
   */
  object DeriveCanBuildFrom {

    /**
     * Derives a `DeriveCanBuildFrom[F]` from the `CanBuildFrom` for `F[Any]`, which the
     * standard collections provide.
     */
    implicit def default[F[+_]](implicit bf: CanBuildFrom[F[Any], Any, F[Any]]): DeriveCanBuildFrom[F] =
      new DeriveCanBuildFrom[F] {
        def derive[A]: CanBuildFrom[F[Any], A, F[A]] =
          bf.asInstanceOf[CanBuildFrom[F[Any], A, F[A]]]
      }
  }
}
