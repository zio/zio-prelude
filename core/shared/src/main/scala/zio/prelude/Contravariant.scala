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

/**
 * Describes a type `F[-_]` that can be contramapped with a function whose
 * input type has an instance of `Subset`. A [[Contravariant]] is a
 * `ContravariantSubset` for every input type.
 */
trait ContravariantSubset[F[-_], Subset[_]] {

  /**
   * Lifts a function from `B` to `A` to a function from `F[A]` to `F[B]`,
   * given a `Subset[B]`.
   */
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
 * Common examples of contravariant instances in ZIO Prelude include functions
 * with regard to their input types, `ZSink` with regard to its input type and
 * `Schedule` with regard to its input type.
 *
 * `Contravariant` instances support a `contramap` operation, which allows
 * transforming the input type given a function from the new input type to the
 * old input type. For example, if we have an `Ord[Int]` that allows us to
 * compare two integers and we have a function `String => Int` that returns
 * the length of a string, then we can construct an `Ord[String]` that
 * compares strings by computing their lengths with the provided function and
 * comparing those.
 *
 * {{{
 * val byLength: Equal[String] =
 *   Contravariant[Equal].contramap((s: String) => s.length)(Equal[Int])
 *
 * val sameLength: Boolean = byLength.equal("ab", "cd")
 * // true
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/functional-abstractions/parameterized-types/contravariant/ Contravariant]]
 */
trait Contravariant[F[-_]] extends ContravariantSubset[F, AnyType] with Invariant[F] { self =>

  /**
   * Returns `contramap(f)`: a `Contravariant` can contramap from every input
   * type.
   */
  final def contramapSubset[A, B: AnyType](f: B => A): F[A] => F[B] =
    contramap(f)

  /**
   * Lift a function from `B` to `A` to a function from `F[A]` to `F[B]`.
   */
  def contramap[A, B](f: B => A): F[A] => F[B]

  /**
   * Lifts an equivalence between `A` and `B` to an equivalence between `F[A]`
   * and `F[B]`, by contramapping with `from` and with `to`.
   */
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

/**
 * Summoner for `Contravariant`. `Contravariant` instances are defined in the
 * companion objects of types such as `Equal`, `Ord` and `Hash`, and, for
 * functions and ZIO types, in the `Invariant` companion object.
 */
object Contravariant {

  /**
   * Summons an implicit `Contravariant[F]`.
   */
  def apply[F[-_]](implicit contravariant: Contravariant[F]): Contravariant[F] =
    contravariant

}

/**
 * Syntax for `Contravariant`, available with `import zio.prelude._`.
 */
trait ContravariantSyntax {

  /**
   * Provides infix syntax for mapping over contravariant values.
   */
  implicit class ContravariantOps[F[-_], A](private val self: F[A]) {

    /**
     * Transforms the input type of this `F` with `f`, which converts the new
     * input type `B` to the old input type `A`.
     */
    def contramap[B](f: B => A)(implicit contravariant: Contravariant[F]): F[B] =
      contravariant.contramap(f)(self)
  }
}
