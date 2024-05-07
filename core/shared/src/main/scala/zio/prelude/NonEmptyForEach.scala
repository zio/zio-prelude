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

import zio.prelude.newtypes.{Max, Min}
import zio.{ChunkBuilder, NonEmptyChunk}

/**
 * A `NonEmptyForEach` describes a `ForEach` that is guaranteed to
 * contain at least one element, such as a `NonEmptyList`, a `NonEmptyChunk`,
 * or certain tree like data structures.
 *
 * Because of the additional information that there is always at least one
 * element, certain operations are available on a `NonEmptyForEach` that
 * are not available on a `ForEach`. For example, if an ordering is
 * defined on the elements of a `NonEmptyForEach` then `min` and `max` are
 * defined, whereas for a `ForEach` only `minOption` and `maxOption` would
 * be, since the collection might not contain any elements at all.
 */
trait NonEmptyForEach[F[+_]] extends ForEach[F] {

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  def forEach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Converts a collection with elements that are in the context of effects to
   * a collection of elements in the context of an effect.
   */
  def flip1[G[+_]: AssociativeBoth: Covariant, A](fa: F[G[A]]): G[F[A]] =
    forEach1(fa)(identity)

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  override def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    forEach1(fa)(f)

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  override def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    forEach1_(fa)(f)

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  def forEach1_[G[+_]: AssociativeBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    forEach1(fa)(f).as(())

  /**
   * Returns the largest value in the collection if one exists or `None`
   * otherwise.
   */
  def max[A: Ord](fa: F[A]): A =
    maxBy(fa)(identity)

  /**
   * Returns the largest element in the collection if one exists, using the
   * function `f` to map each element to a type for which an `Ord` is defined,
   * or `None` otherwise.
   */
  def maxBy[A, B: Ord](fa: F[A])(f: A => B): A = {
    implicit val ord: Ord[A] = Ord[B].contramap(f)
    reduceMap(fa)(a => Max[A](a))
  }

  /**
   * Returns the smallest value in the collection if one exists or `None`
   * otherwise.
   */
  def min[A: Ord](fa: F[A]): A =
    minBy(fa)(identity)

  /**
   * Returns the smallest element in the collection if one exists, using the
   * function `f` to map each element to a type for which an `Ord` is defined,
   * or `None` otherwise.
   */
  def minBy[A, B: Ord](fa: F[A])(f: A => B): A = {
    implicit val ord: Ord[A] = Ord[B].contramap(f)
    reduceMap(fa)(a => Min[A](a))
  }

  /**
   * Reduces the collection to a summary value using the binary function `f`.
   */
  def reduceAll[A](fa: F[A])(f: (A, A) => A): A = {
    implicit val associative: Associative[A] = Associative.make(f)
    reduceMap(fa)(identity)
  }

  /**
   * Reduces the non-empty collection of associative elements.
   */
  def reduce1[A: Associative](fa: F[A]): A =
    reduceMap(fa)(identity)

  /**
   * Reduces the collection to a summary value using the idempotent operation,
   * returning `None` if the collection is empty.
   */
  def reduceIdempotent1[A: Idempotent: Equal](fa: F[A]): A =
    reduce1(fa)(Idempotent[A].idempotent)

  /**
   * Maps each element of the collection to a type `B` for which a combine
   * operation is defined using the function `f` and then reduces those values
   * to a single summary using the combine operation.
   */
  def reduceMap[A, B: Associative](fa: F[A])(f: A => B): B =
    reduceMapLeft(fa)(f)((b, a) => Associative[B].combine(b, f(a)))

  /**
   * Reduces the elements of this collection from left to right using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduceAll` to combine the `B` value with each other `A` value.
   */
  def reduceMapLeft[A, B](fa: F[A])(map: A => B)(reduce: (B, A) => B): B = {
    type StateB[+A] = State[Option[B], A]
    forEach[StateB, A, Any](fa) { a =>
      State.update {
        case None    => Some(map(a))
        case Some(b) => Some(reduce(b, a))
      }
    }.runState(None).get
  }

  /**
   * Reduces the elements of this collection from right to left using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduceAll` to combine the `B` value with each other `A` value.
   */
  def reduceMapRight[A, B](fa: F[A])(map: A => B)(reduce: (A, B) => B): B =
    reduceMapLeft(reverse(fa))(map)((b, a) => reduce(a, b))

  /**
   * Converts the collection to a `NonEmptyChunk`.
   */
  def toNonEmptyChunk[A](fa: F[A]): NonEmptyChunk[A] =
    NonEmptyChunk.nonEmpty(reduceMapLeft(fa)(ChunkBuilder.make[A]() += _)(_ += _).result())

  /**
   * Converts the collection to a `NonEmptyList`.
   */
  def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] =
    reduceMapLeft(fa)(NonEmptyList.single)((as, a) => NonEmptyList.cons(a, as)).reverse
}
object NonEmptyForEach {

  /**
   * Summons an implicit `NonEmptyForEach`.
   */
  def apply[F[+_]](implicit nonEmptyForEach: NonEmptyForEach[F]): NonEmptyForEach[F] =
    nonEmptyForEach
}

trait NonEmptyForEachSyntax {

  /**
   * Provides infix syntax for traversing collections.
   */
  implicit class NonEmptyForEachOps[F[+_], A](private val self: F[A]) {

    /**
     * Traverse each element in the collection using the specified effectual
     * function `f`, returning a new collection with the results in the context
     * of the effect.
     */
    def forEach1[G[+_]: AssociativeBoth: Covariant, B](f: A => G[B])(implicit F: NonEmptyForEach[F]): G[F[B]] =
      F.forEach1(self)(f)

    /**
     * Traverses each element in the collection with the specified effectual
     * function `f` purely for its effects.
     */
    def forEach1_[G[+_]: AssociativeBoth: Covariant](f: A => G[Any])(implicit F: NonEmptyForEach[F]): G[Unit] =
      F.forEach1_(self)(f)

    /**
     * Reduces the collection to a summary value using the binary function `f`.
     */
    def reduceAll(f: (A, A) => A)(implicit F: NonEmptyForEach[F]): A =
      F.reduceAll(self)(f)

    /**
     * Reduces the non-empty collection of associative elements.
     */
    def reduce1(implicit F: NonEmptyForEach[F], A: Associative[A]): A =
      F.reduce1(self)

    /**
     * Reduces the collection to a summary value using the idempotent operation,
     * returning `None` if the collection is empty.
     */
    def reduceIdempotent1(implicit F: NonEmptyForEach[F], ia: Idempotent[A], ea: Equal[A]): A =
      F.reduceIdempotent1(self)

    /**
     * Maps each element of the collection to a type `B` for which a combine
     * operation is defined using the function `f` and then reduces those values
     * to a single summary using the combine operation.
     */
    def reduceMap[B: Associative](f: A => B)(implicit F: NonEmptyForEach[F]): B =
      F.reduceMap(self)(f)

    /**
     * Reduces the elements of this collection from left to right using the
     * function `map` to transform the first value to the type `B` and then the
     * function `reduceAll` to combine the `B` value with each other `A` value.
     */
    def reduceMapLeft[B](map: A => B)(reduce: (B, A) => B)(implicit F: NonEmptyForEach[F]): B =
      F.reduceMapLeft(self)(map)(reduce)

    /**
     * Reduces the elements of this collection from right to left using the
     * function `map` to transform the first value to the type `B` and then the
     * function `reduceAll` to combine the `B` value with each other `A` value.
     */
    def reduceMapRight[B](map: A => B)(reduce: (A, B) => B)(implicit F: NonEmptyForEach[F]): B =
      F.reduceMapRight(self)(map)(reduce)

    /**
     * Converts the collection to a `NonEmptyChunk`.
     */
    def toNonEmptyChunk(implicit F: NonEmptyForEach[F]): NonEmptyChunk[A] =
      F.toNonEmptyChunk(self)

    /**
     * Converts the collection to a `NonEmptyList`.
     */
    def toNonEmptyList(implicit F: NonEmptyForEach[F]): NonEmptyList[A] =
      F.toNonEmptyList(self)
  }

  /**
   * Provides infix syntax for flip1.
   */
  implicit class Flip1Ops[F[+_], G[+_], A](private val self: F[G[A]]) {

    /**
     * Converts a collection with elements that are in the context of effects to
     * a collection of elements in the context of an effect.
     */
    def flip1[B](implicit
      nonEmptyForEach: NonEmptyForEach[F],
      associativeBoth: AssociativeBoth[G],
      covariant: Covariant[G]
    ): G[F[A]] =
      nonEmptyForEach.flip1(self)
  }
}
