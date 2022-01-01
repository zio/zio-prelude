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

trait CovariantSubset[F[+_], Subset[_]] {
  def mapSubset[A, B: Subset](f: A => B): F[A] => F[B]
}

/**
 * `Covariant[F]` provides implicit evidence that `F[+_]` is a covariant
 * endofunctor in the category of Scala objects.
 *
 * Covariant instances of type `F[A]` "produce" values of type `A` in some
 * sense. In some cases, such as with a `List[A]`, this means that they
 * contain values of type `A`, in which case we can simply access the elements
 * of the collection. In other cases it means that output values of type `A`
 * which may not already exists, such as with a `Function0[A]` that produces
 * `A` values when invoked.
 *
 * Common examples of covariant instances in ZIO includes effects with respect
 * to their error and value types, sinks with respect to their error and output
 * types, and queues and references with respect to their error and
 * output types.
 *
 * `Covariant` instances support a `map` operation which allows transforming
 * the output type given a function from the old output type to the new output
 * type. For example, if we have a `List[String]` and a function
 * `String => Int` that returns the length of a string, then we can construct
 * a `List[Int]` with the length of each string.
 */
trait Covariant[F[+_]] extends CovariantSubset[F, AnyType] with Invariant[F] { self =>
  final def mapSubset[A, B: AnyType](f: A => B): F[A] => F[B] = map(f)

  /**
   * Lift a function from `A` to `B` to a function from `F[A]` to `F[B]`.
   */
  def map[A, B](f: A => B): F[A] => F[B]

  def fproduct[A, B](f: A => B): F[A] => F[(A, B)] = map(a => a -> f(a))

  def fproductLeft[A, B](f: A => B): F[A] => F[(B, A)] = map(a => f(a) -> a)

  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => map(f.to)(fa), (fb: F[B]) => map(f.from)(fb))

  /**
   * Compose two covariant functors.
   */
  final def compose[G[+_]](implicit g: Covariant[G]): Covariant[({ type lambda[+A] = F[G[A]] })#lambda] =
    new Covariant[({ type lambda[+A] = F[G[A]] })#lambda] {
      def map[A, B](f: A => B): F[G[A]] => F[G[B]] = self.map(g.map(f))
    }

  /**
   * Compose covariant and contravariant functors.
   */
  final def compose[G[-_]](implicit g: Contravariant[G]): Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] =
    new Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] {
      def contramap[A, B](f: B => A): F[G[A]] => F[G[B]] = self.map(g.contramap(f))
    }
}

object Covariant {

  /**
   * Summons an implicit `Covariant[F]`.
   */
  def apply[F[+_]](implicit covariant: Covariant[F]): Covariant[F] =
    covariant

}

trait CovariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class CovariantOps[F[+_], A](private val self: F[A]) {
    def as[B](b: => B)(implicit F: Covariant[F]): F[B] = map(_ => b)

    def map[B](f: A => B)(implicit F: Covariant[F]): F[B] =
      F.map(f)(self)

    def fproduct[B](f: A => B)(implicit F: Covariant[F]): F[(A, B)] =
      F.fproduct[A, B](f)(self)

    def fproductLeft[B](f: A => B)(implicit F: Covariant[F]): F[(B, A)] =
      F.fproductLeft[A, B](f)(self)

    def unit(implicit F: Covariant[F]): F[Unit] = as(())
  }
}
