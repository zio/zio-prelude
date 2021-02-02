/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

import zio.prelude.coherent.DeriveEqualForEach
import zio.prelude.newtypes.{And, First, Max, Min, Or, Prod, Sum}
import zio.test.laws._
import zio.{Chunk, ChunkBuilder, NonEmptyChunk}

/**
 * `ForEach` is an abstraction that describes the ability to iterate over
 * a collection, performing an effect for each element in the collection and
 * returning a collection with the same shape in the context of the effect.
 *
 * By choosing the appropriate effect type to traverse with a wide range of
 * operations on collections can be described. In particular, by traversing
 * with state we can describe folds which allow implementing a wide variety of
 * collection operations that produce summaries from a collection of values.
 */
trait ForEach[F[+_]] extends Covariant[F] { self =>

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Returns whether the collection contains the specified element.
   */
  def contains[A, A1 >: A](fa: F[A])(a: A1)(implicit A: Equal[A1]): Boolean =
    exists(fa)(_ === a)

  /**
   * Returns the number of elements in the collection that satisfy the
   * specified predicate.
   */
  def count[A](fa: F[A])(f: A => Boolean): Int =
    foldMap(fa)(a => if (f(a)) Sum(1) else Sum(0))

  /**
   * Returns whether any element of the collection satisfies the specified
   * predicate.
   */
  def exists[A](fa: F[A])(f: A => Boolean): Boolean =
    foldMap(fa)(a => Or(f(a)))

  /**
   * Returns the first element in the collection satisfying the specified
   * predicate if one exists or `None` otherwise.
   */
  def find[A](fa: F[A])(f: A => Boolean): Option[A] =
    First.unwrapAll(foldMap(fa)(a => if (f(a)) Some(First(a)) else None))

  /**
   * Converts a collection with elements that are in the context of effects to
   * a collection of elements in the context of an effect.
   */
  def flip[G[+_]: IdentityBoth: Covariant, A](fa: F[G[A]]): G[F[A]] =
    forEach(fa)(identity)

  /**
   * Folds over the elements of this collection using an associative operation
   * and an identity.
   */
  def fold[A: Identity](fa: F[A]): A =
    foldMap(fa)(identity)

  /**
   * Folds over the elements of this collection from left to right to produce a
   * summary value, maintaining some internal state along the way.
   */
  def foldLeft[S, A](fa: F[A])(s: S)(f: (S, A) => S): S                                           =
    forEach[({ type lambda[+A] = State[S, A] })#lambda, A, Unit](fa)(a => State.update((s: S) => f(s, a))).runState(s)

  /**
   * Effectually fold over the elements of this collection from left to right
   * to produce a summary value, maintaining some internal state along the way.
   */
  def foldLeftM[G[+_]: IdentityFlatten: Covariant, S, A](fa: F[A])(s: S)(f: (S, A) => G[S]): G[S] =
    foldLeft[G[S], A](fa)(IdentityFlatten[G].any.map(_ => s))((s, a) => s.flatMap(f(_, a)))

  /**
   * Maps each element of the collection to a type `B` for which an `Identity`
   * is defined using the function `f`, then reduces those values to a single
   * summary using the `combine` operation of `Identity`, or the `identity`
   * element if the collection is empty.
   */
  def foldMap[A, B: Identity](fa: F[A])(f: A => B): B =
    foldLeft(fa)(Identity[B].identity)((b: B, a: A) => b combine f(a))

  /**
   * Folds over the elements of this collection from right to left to produce a
   * summary value, maintaining some internal state along the way.
   */
  def foldRight[S, A](fa: F[A])(s: S)(f: (A, S) => S): S =
    foldLeft(reverse(fa))(s)((s, a) => f(a, s))

  /**
   * Effectually fold over the elements of this collection from right to left
   * to produce a summary value, maintaining some internal state along the way.
   */
  def foldRightM[G[+_]: IdentityFlatten: Covariant, S, A](fa: F[A])(s: S)(f: (A, S) => G[S]): G[S] =
    foldRight[G[S], A](fa)(IdentityFlatten[G].any.map(_ => s))((a, s) => s.flatMap(f(a, _)))

  /**
   * Returns whether any element of the collection satisfies the specified
   * predicate.
   */
  def forall[A](fa: F[A])(f: A => Boolean): Boolean =
    foldMap(fa)(a => And(f(a)))

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    forEach(fa)(f).as(())

  def groupBy[V, K](fa: F[V])(f: V => K): Map[K, NonEmptyChunk[V]] =
    foldLeft(fa)(Map.empty[K, NonEmptyChunk[V]]) { (m, v) =>
      val k = f(v)
      m.get(k) match {
        case Some(vs) => m + (k -> (vs :+ v))
        case None     => m + (k -> NonEmptyChunk(v))
      }
    }

  def groupByM[G[+_]: IdentityBoth: Covariant, V, K](fa: F[V])(f: V => G[K]): G[Map[K, NonEmptyChunk[V]]] =
    foldLeft(fa)(Map.empty[K, NonEmptyChunk[V]].succeed) { (m, v) =>
      val k = f(v)
      AssociativeBoth.mapN(m, k) { (m, k) =>
        m.get(k) match {
          case Some(vs) => m + (k -> (vs :+ v))
          case None     => m + (k -> NonEmptyChunk(v))
        }
      }
    }

  /**
   * Returns whether the collection is empty.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    foldMap(fa)(_ => And(false))

  /**
   * Lifts a function operating on values to a function that operates on each
   * element of a collection.
   */
  def map[A, B](f: A => B): F[A] => F[B] =
    (fa: F[A]) => Id.unwrap(forEach[Id, A, B](fa)((a: A) => Id(f(a))))

  /**
   * Statefully maps over the elements of the collection, maintaining some
   * state along the way and returning the final state along with the new
   * collection.
   */
  def mapAccum[S, A, B](fa: F[A])(s: S)(f: (S, A) => (S, B)): (S, F[B]) =
    forEach[({ type lambda[+A] = State[S, A] })#lambda, A, B](fa)(a => State.modify((s: S) => f(s, a))).run(s)

  /**
   * Returns the largest value in the collection if one exists or `None`
   * otherwise.
   */
  def maxOption[A: Ord](fa: F[A]): Option[A]                            =
    maxByOption(fa)(identity)

  /**
   * Returns the largest element in the collection if one exists, using the
   * function `f` to map each element to a type for which an `Ord` is defined,
   * or `None` otherwise.
   */
  def maxByOption[A, B: Ord](fa: F[A])(f: A => B): Option[A] = {
    implicit val ord: Ord[A] = Ord[B].contramap(f)
    Max.unwrapAll(reduceMapOption(fa)(a => Max(a)))
  }

  /**
   * Returns the smallest value in the collection if one exists or `None`
   * otherwise.
   */
  def minOption[A: Ord](fa: F[A]): Option[A] =
    minByOption(fa)(identity)

  /**
   * Returns the smallest element in the collection if one exists, using the
   * function `f` to map each element to a type for which an `Ord` is defined,
   * or `None` otherwise.
   */
  def minByOption[A, B: Ord](fa: F[A])(f: A => B): Option[A] = {
    implicit val ord: Ord[A] = Ord[B].contramap(f)
    Min.unwrapAll(reduceMapOption(fa)(a => Min(a)))
  }

  /**
   * Returns whether the collection contains at least one element
   */
  def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  /**
   * Returns the product of all elements in the collection.
   */
  def product[A](fa: F[A])(implicit ev: Identity[Prod[A]]): A =
    foldMap(fa)(Prod[A])

  /**
   * Reduces the collection to a summary value using the associative operation,
   * returning `None` if the collection is empty.
   */
  def reduceAssociative[A: Associative](fa: F[A]): Option[A] =
    foldMap(fa)(a => Option(a))

  /**
   * Reduces the collection to a summary value using the idempotent operation,
   * returning `None` if the collection is empty.
   */
  def reduceIdempotent[A: Idempotent: Equal](fa: F[A]): Option[A] =
    reduceAssociative(fa)(Idempotent[A].idempotent)

  /**
   * Reduces the collection to a summary value using the associative operation.
   */
  def reduceIdentity[A: Identity](fa: F[A]): A =
    foldMap(fa)(identity[A])

  /**
   * Maps each element of the collection to a type `B` for which an
   * associative operation exists and then reduces the values using the
   * associative operation, returning `None` if the collection is empty.
   */
  def reduceMapOption[A, B: Associative](fa: F[A])(f: A => B): Option[B] =
    foldMap(fa)(a => Option(f(a)))

  /**
   * Reduces the collection to a summary value using the binary function `f`,
   * returning `None` if the collection is empty.
   */
  def reduceOption[A](fa: F[A])(f: (A, A) => A): Option[A] = {
    implicit val associative: Associative[A] = Associative.make(f)
    reduceMapOption(fa)(identity)
  }

  /**
   * Reverses the order of elements in the collection.
   */
  def reverse[A](fa: F[A]): F[A] = {
    val reversed = foldLeft(fa)(List.empty[A])((as, a) => a :: as)
    mapAccum(fa)(reversed)((as, _) => (as.tail, as.head))._2
  }

  /**
   * Returns the number of elements in the collection.
   */
  def size[A](fa: F[A]): Int =
    foldMap(fa)(_ => Sum(1))

  /**
   * Returns the sum of all elements in the collection.
   */
  def sum[A](fa: F[A])(implicit ev: Identity[Sum[A]]): A =
    foldMap(fa)(Sum[A])

  /**
   * Converts the collection to a `Chunk`.
   */
  def toChunk[A](fa: F[A]): Chunk[A] =
    foldLeft(fa)(ChunkBuilder.make[A]())((builder, a) => builder += a).result()

  /**
   * Converts the collection to a `List`.
   */
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa)(List.empty[A])((as, a) => a :: as).reverse

  /**
   * Zips each element of the collection with its index.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    forEach[({ type lambda[+A] = State[Int, A] })#lambda, A, (A, Int)](fa)(a =>
      State.modify((n: Int) => (n + 1, (a, n)))
    ).runResult(0)

  final def compose[G[+_]: ForEach]: ForEach[({ type lambda[+A] = F[G[A]] })#lambda] =
    new ForEach[({ type lambda[+A] = F[G[A]] })#lambda] {
      def forEach[H[+_]: IdentityBoth: Covariant, A, B](fa: F[G[A]])(f: A => H[B]) =
        self.forEach(fa)(_.forEach(f))
    }
}

object ForEach extends LawfulF.Covariant[DeriveEqualForEach, Equal] {

  /**
   * The set of all laws that instances of `ForEach` must satisfy.
   */
  val laws: LawsF.Covariant[DeriveEqualForEach, Equal] =
    Covariant.laws

  /**
   * Summons an implicit `ForEach`.
   */
  def apply[F[+_]](implicit forEach: ForEach[F]): ForEach[F] =
    forEach
}

trait ForEachSyntax {

  /**
   * Provides infix syntax for traversing collections.
   */
  implicit class ForEachOps[F[+_], A](private val self: F[A]) {
    def forEach[G[+_]: IdentityBoth: Covariant, B](f: A => G[B])(implicit F: ForEach[F]): G[F[B]]               =
      F.forEach(self)(f)
    def contains[A1 >: A](a: A1)(implicit A: Equal[A1], F: ForEach[F]): Boolean                                 =
      F.contains[A, A1](self)(a)
    def count(f: A => Boolean)(implicit F: ForEach[F]): Int                                                     =
      F.count(self)(f)
    def exists(f: A => Boolean)(implicit F: ForEach[F]): Boolean                                                =
      F.exists(self)(f)
    def find(f: A => Boolean)(implicit F: ForEach[F]): Option[A]                                                =
      F.find(self)(f)
    def foldLeft[S](s: S)(f: (S, A) => S)(implicit F: ForEach[F]): S                                            =
      F.foldLeft(self)(s)(f)
    def foldLeftM[G[+_]: IdentityFlatten: Covariant, S](s: S)(f: (S, A) => G[S])(implicit F: ForEach[F]): G[S]  =
      F.foldLeftM(self)(s)(f)
    def foldMap[B: Identity](f: A => B)(implicit F: ForEach[F]): B                                              =
      F.foldMap(self)(f)
    def foldRight[S](s: S)(f: (A, S) => S)(implicit F: ForEach[F]): S                                           =
      F.foldRight(self)(s)(f)
    def foldRightM[G[+_]: IdentityFlatten: Covariant, S](s: S)(f: (A, S) => G[S])(implicit F: ForEach[F]): G[S] =
      F.foldRightM(self)(s)(f)
    def forall(f: A => Boolean)(implicit F: ForEach[F]): Boolean                                                =
      F.forall(self)(f)
    def forEach_[G[+_]: IdentityBoth: Covariant](f: A => G[Any])(implicit F: ForEach[F]): G[Unit]               =
      F.forEach_(self)(f)
    def isEmpty(implicit F: ForEach[F]): Boolean                                                                =
      F.isEmpty(self)
    def mapAccum[S, B](s: S)(f: (S, A) => (S, B))(implicit F: ForEach[F]): (S, F[B])                            =
      F.mapAccum(self)(s)(f)
    def maxOption(implicit A: Ord[A], F: ForEach[F]): Option[A]                                                 =
      F.maxOption(self)
    def maxByOption[B: Ord](f: A => B)(implicit F: ForEach[F]): Option[A]                                       =
      F.maxByOption(self)(f)
    def minOption(implicit A: Ord[A], F: ForEach[F]): Option[A]                                                 =
      F.minOption(self)
    def minByOption[B: Ord](f: A => B)(implicit F: ForEach[F]): Option[A]                                       =
      F.minByOption(self)(f)
    def nonEmpty(implicit F: ForEach[F]): Boolean                                                               =
      F.nonEmpty(self)
    def reduceAssociative(implicit F: ForEach[F], A: Associative[A]): Option[A]                                 =
      F.reduceAssociative(self)
    def reduceIdempotent(implicit F: ForEach[F], ia: Idempotent[A], ea: Equal[A]): Option[A]                    =
      F.reduceIdempotent(self)
    def reduceIdentity(implicit F: ForEach[F], A: Identity[A]): A                                               =
      F.reduceIdentity(self)
    def product(implicit A: Identity[Prod[A]], F: ForEach[F]): A                                                =
      F.product(self)
    def reduceMapOption[B: Associative](f: A => B)(implicit F: ForEach[F]): Option[B]                           =
      F.reduceMapOption(self)(f)
    def reduceOption(f: (A, A) => A)(implicit F: ForEach[F]): Option[A]                                         =
      F.reduceOption(self)(f)
    def reverse(implicit F: ForEach[F]): F[A]                                                                   =
      F.reverse(self)
    def size(implicit F: ForEach[F]): Int                                                                       =
      F.size(self)
    def sum(implicit A: Identity[Sum[A]], F: ForEach[F]): A                                                     =
      F.sum(self)
    def toChunk(implicit F: ForEach[F]): Chunk[A]                                                               =
      F.toChunk(self)
    def zipWithIndex(implicit F: ForEach[F]): F[(A, Int)]                                                       =
      F.zipWithIndex(self)
  }

  /**
   * Provides infix syntax for flip.
   */
  implicit class FlipOps[F[+_], G[+_], A](private val self: F[G[A]]) {
    def flip[B](implicit forEach: ForEach[F], identityBoth: IdentityBoth[G], covariant: Covariant[G]): G[F[A]] =
      forEach.flip(self)
  }
}
