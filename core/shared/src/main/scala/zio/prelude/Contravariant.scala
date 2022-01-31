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

trait ContravariantSubset[F[-_], Subset[_]] {
  def contramapSubset[A, B: Subset](f: B => A): F[A] => F[B]
}

/**
 * `Contravariant[F]` provides implicit evidence that `F[-_]` is a
 * contravariant endofunctor in the category of Scala objects.
 *
 * `Contravariant` instances of type `F[A]` "consume" values of type `A` in
 * some sense. For example, `Equal[A]` takes two values of type `A` as input
 * and returns a `Boolean` indicating whether they are equal. Similarly, a
 * `Ord[A]` takes two values of type `A` as input and returns an `Ordering`
 * with the result of comparing them and `Hash` takes an `A` value and returns
 * an `Int`.
 *
 * Common examples of contravariant instances in ZIO include effects with
 * regard to their environment types, sinks with regard to their input type,
 * and polymorphic queues and references regarding their input types.
 *
 * `Contravariant` instances support a `contramap` operation, which allows
 * transforming the input type given a function from the new input type to the
 * old input type. For example, if we have an `Ord[Int]` that allows us to
 * compare two integers and we have a function `String => Int` that returns
 * the length of a string, then we can construct an `Ord[String]` that
 * compares strings by computing their lengths with the provided function and
 * comparing those.
 */
trait Contravariant[F[-_]] extends ContravariantSubset[F, AnyType] with Invariant[F] { self =>
  final def contramapSubset[A, B: AnyType](f: B => A): F[A] => F[B] =
    contramap(f)

  /**
   * Lift a function from `B` to `A` to a function from `F[A]` to `F[B]`.
   */
  def contramap[A, B](f: B => A): F[A] => F[B]

  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => contramap(f.from)(fa), (fb: F[B]) => contramap(f.to)(fb))

  /**
   * Compose two contravariant functors.
   */
  final def compose[G[-_]](implicit g: Contravariant[G]): Covariant[({ type lambda[+A] = F[G[A]] })#lambda] =
    new Covariant[({ type lambda[+A] = F[G[A]] })#lambda] {
      def map[A, B](f: A => B): F[G[A]] => F[G[B]] = self.contramap(g.contramap(f))
    }

  /**
   * Compose contravariant and covariant functors.
   */
  final def compose[G[+_]](implicit g: Covariant[G]): Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] =
    new Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] {
      def contramap[A, B](f: B => A): F[G[A]] => F[G[B]] = self.contramap(g.map(f))
    }
}

object Contravariant {

  /**
   * Summons an implicit `Contravariant[F]`.
   */
  def apply[F[-_]](implicit contravariant: Contravariant[F]): Contravariant[F] =
    contravariant

}

trait ContravariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class ContravariantOps[F[-_], A](private val self: F[A]) {
    def contramap[B](f: B => A)(implicit contravariant: Contravariant[F]): F[B] =
      contravariant.contramap(f)(self)
  }
}
