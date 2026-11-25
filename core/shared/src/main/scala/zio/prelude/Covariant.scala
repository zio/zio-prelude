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
 * Describes a type `F[+_]` that can be mapped over with a function whose
 * result type has an instance of `Subset`, such as a type with an `Ord` for a
 * sorted collection. A [[Covariant]] is a `CovariantSubset` for every result
 * type.
 */
trait CovariantSubset[F[+_], Subset[_]] {

  /**
   * Lifts a function from `A` to `B` to a function from `F[A]` to `F[B]`,
   * given a `Subset[B]`.
   */
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
 * which may not already exist, such as with a `Function0[A]` that produces
 * `A` values when invoked.
 *
 * Common examples of covariant instances in ZIO include `ZIO`, `ZSTM` and
 * `ZStream` with respect to their value types, and with respect to their
 * error types through the `Failure` newtype, as well as `Exit`, `Fiber`,
 * `Cause` and the output type of `Schedule`.
 *
 * `Covariant` instances support a `map` operation which allows transforming
 * the output type given a function from the old output type to the new output
 * type. For example, if we have a `List[String]` and a function
 * `String => Int` that returns the length of a string, then we can construct
 * a `List[Int]` with the length of each string.
 *
 * {{{
 * val lengths: List[Int] = Covariant[List].map((s: String) => s.length)(List("a", "bb"))
 * // List(1, 2)
 *
 * val pairs: Option[(String, Int)] = Option("zio").fproduct(_.length)
 * // Some((zio,3))
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/functional-abstractions/parameterized-types/covariant/ Covariant]]
 */
trait Covariant[F[+_]] extends CovariantSubset[F, AnyType] with Invariant[F] { self =>

  /**
   * Returns `map(f)`: a `Covariant` can map to every result type.
   */
  final def mapSubset[A, B: AnyType](f: A => B): F[A] => F[B] = map(f)

  /**
   * Lift a function from `A` to `B` to a function from `F[A]` to `F[B]`.
   * Mapping with the identity function must return an equal value, and
   * mapping with `f` and then `g` must equal mapping with `f andThen g`.
   */
  def map[A, B](f: A => B): F[A] => F[B]

  /**
   * Lifts a function from `A` to `B` to a function that pairs each `A` with
   * its result: `F[A]` to `F[(A, B)]`.
   */
  def fproduct[A, B](f: A => B): F[A] => F[(A, B)] = map(a => a -> f(a))

  /**
   * Lifts a function from `A` to `B` to a function that pairs each result
   * with its `A`, result first: `F[A]` to `F[(B, A)]`.
   */
  def fproductLeft[A, B](f: A => B): F[A] => F[(B, A)] = map(a => f(a) -> a)

  /**
   * Lifts an equivalence between `A` and `B` to an equivalence between `F[A]`
   * and `F[B]`, by mapping with `to` and with `from`.
   */
  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => map(f.to)(fa), (fb: F[B]) => map(f.from)(fb))

  /**
   * Compose two covariant functors. The result maps the values inside an
   * `F[G[A]]`.
   */
  final def compose[G[+_]](implicit g: Covariant[G]): Covariant[({ type lambda[+A] = F[G[A]] })#lambda] =
    new Covariant[({ type lambda[+A] = F[G[A]] })#lambda] {
      def map[A, B](f: A => B): F[G[A]] => F[G[B]] = self.map(g.map(f))
    }

  /**
   * Compose covariant and contravariant functors. The result is contravariant
   * in the type inside an `F[G[A]]`.
   */
  final def compose[G[-_]](implicit g: Contravariant[G]): Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] =
    new Contravariant[({ type lambda[-A] = F[G[A]] })#lambda] {
      def contramap[A, B](f: B => A): F[G[A]] => F[G[B]] = self.map(g.contramap(f))
    }
}

/**
 * Summoner for `Covariant`. The `Covariant` instances for standard library
 * and ZIO types are in the `Invariant` companion object.
 */
object Covariant {

  /**
   * Summons an implicit `Covariant[F]`.
   */
  def apply[F[+_]](implicit covariant: Covariant[F]): Covariant[F] =
    covariant

}

/**
 * Syntax for `Covariant`, available with `import zio.prelude._`.
 */
trait CovariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values. A type's own
   * method with the same name, such as `List#map`, takes precedence.
   */
  implicit class CovariantOps[F[+_], A](private val self: F[A]) {

    /**
     * Replaces every value inside this `F` with `b`.
     *
     * {{{
     * val replaced: Option[String] = Option(1).as("done")
     * // Some(done)
     * }}}
     */
    def as[B](b: => B)(implicit F: Covariant[F]): F[B] = map(_ => b)

    /**
     * Transforms the values inside this `F` with `f`.
     */
    def map[B](f: A => B)(implicit F: Covariant[F]): F[B] =
      F.map(f)(self)

    /**
     * Pairs each value inside this `F` with the result of `f` on it.
     */
    def fproduct[B](f: A => B)(implicit F: Covariant[F]): F[(A, B)] =
      F.fproduct[A, B](f)(self)

    /**
     * Pairs the result of `f` on each value inside this `F` with that value,
     * result first.
     */
    def fproductLeft[B](f: A => B)(implicit F: Covariant[F]): F[(B, A)] =
      F.fproductLeft[A, B](f)(self)

    /**
     * Replaces every value inside this `F` with `()`.
     */
    def unit(implicit F: Covariant[F]): F[Unit] = as(())
  }
}
