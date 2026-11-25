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

import zio.prelude.newtypes._
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
 *
 * {{{
 * ForEach[List].forEach(List(1, 2, 3))(n => if (n > 0) Some(n) else None) // Some(List(1, 2, 3))
 * ForEach[List].forEach(List(1, -2, 3))(n => if (n > 0) Some(n) else None) // None
 * ForEach[List].zipWithIndex(List("a", "b"))                                // List((a,0), (b,1))
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/functional-abstractions/parameterized-types/foreach/ ForEach]]
 */
trait ForEach[F[+_]] extends Covariant[F] { self =>

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Collects the results of the partial function `pf` for the elements on
   * which it is defined.
   */
  def collect[A, B](
    fa: F[A]
  )(pf: PartialFunction[A, B])(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): F[B] =
    Id.unwrap(collectM(fa)(pf.lift.andThen(Id(_))))

  /**
   * Runs the effectual function `f` on each element and collects the values
   * it returns in `Some`, dropping the elements for which it returns `None`.
   */
  def collectM[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(
    f: A => G[Option[B]]
  )(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): G[F[B]] = {
    implicit val covariant: Covariant[F]  = self
    implicit val identity: Identity[F[B]] = Identity.fromIdentityEitherCovariant
    foldMapM(fa) { a =>
      f(a).map {
        case Some(b) => b.succeed[F]
        case None    => identityEither.none
      }
    }
  }

  /**
   * Reduces the collection to a summary value using the `Identity` instance
   * of `A`, or returns the identity element if the collection is empty. Alias
   * for `fold`.
   */
  def concatenate[A: Identity](fa: F[A]): A =
    foldMap(fa)(identity[A])

  /**
   * Returns whether the collection contains the specified element, compared
   * with the `Equal` instance.
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
   * predicate. Returns `false` for an empty collection.
   */
  def exists[A](fa: F[A])(f: A => Boolean): Boolean =
    foldMap(fa)(a => Or.create(f(a)))

  /**
   * Filters the collection with the predicate `f`.
   */
  def filter[A](
    fa: F[A]
  )(f: A => Boolean)(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): F[A] =
    Id.unwrap(filterM(fa)(a => Id(f(a))))

  /**
   * Filters the collection with the effectual predicate `f`.
   */
  def filterM[G[+_]: IdentityBoth: Covariant, A](
    fa: F[A]
  )(f: A => G[Boolean])(implicit identityBoth: IdentityBoth[F], identityEither: IdentityEither[F]): G[F[A]] =
    collectM(fa)(a => f(a).map(b => if (b) Some(a) else None))

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
   * and an identity, and returns the identity element if the collection is
   * empty. Alias for `reduceIdentity`.
   */
  def fold[A: Identity](fa: F[A]): A =
    foldMap(fa)(identity)

  /**
   * Folds over the elements of this collection from left to right to produce a
   * summary value, maintaining some internal state along the way.
   */
  def foldLeft[S, A](fa: F[A])(s: S)(f: (S, A) => S): S =
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
  def foldMap[A, B: Identity](fa: F[A])(f: A => B): B = {
    type ConstB[+A] = Const[B, A]
    Const.unwrap(forEach[ConstB, A, B](fa)(a => Const(f(a))))
  }

  /**
   * Effectfully maps each element of the collection to a type `B` for which an
   * `Identity` is defined using the function `f`, then reduces those values to
   * a single summary using the `combine` operation of `Identity`, or the
   * `identity` element if the collection is empty.
   */
  def foldMapM[G[+_]: Covariant: IdentityBoth, A, B: Identity](fa: F[A])(f: A => G[B]): G[B] =
    forEach(fa)(f).map(fold[B])

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
   * Returns whether every element of the collection satisfies the specified
   * predicate. Returns `true` for an empty collection.
   */
  def forall[A](fa: F[A])(f: A => Boolean): Boolean =
    foldMap(fa)(a => And.create(f(a)))

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  def forEach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    forEach(fa)(f).as(())

  /**
   * Traverses the collection with the effectual function `f`, which returns a
   * collection for each element, and flattens the results into one
   * collection inside the effect.
   */
  def forEachFlatten[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[F[B]])(implicit
    F: AssociativeFlatten[F]
  ): G[F[B]] =
    forEach(fa)(f).map(_.flatten)

  /**
   * Groups the elements by the key that `f` returns. Each group is a
   * `NonEmptyChunk` that keeps the elements in their original order; an
   * empty collection gives an empty `Map`.
   */
  def groupByNonEmpty[V, K](fa: F[V])(f: V => K): Map[K, NonEmptyChunk[V]] =
    foldLeft(fa)(Map.empty[K, NonEmptyChunk[V]]) { (m, v) =>
      val k = f(v)
      m.get(k) match {
        case Some(vs) => m + (k -> (vs :+ v))
        case None     => m + (k -> NonEmptyChunk(v))
      }
    }

  /**
   * Groups the elements by the key that the effectual function `f` returns.
   * Each group is a `NonEmptyChunk` that keeps the elements in their original
   * order.
   */
  def groupByNonEmptyM[G[+_]: IdentityBoth: Covariant, V, K](fa: F[V])(f: V => G[K]): G[Map[K, NonEmptyChunk[V]]] =
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
    foldMap(fa)(_ => And.create(false))

  /**
   * Folds over the elements of this collection using the `Identity` instance,
   * with `middle` combined in between each pair of elements. Returns the
   * identity element if the collection is empty.
   */
  def intersperse[A](fa: F[A], middle: A)(implicit I: Identity[A]): A =
    reduceAssociative(fa)(I.intersperse(middle)) match {
      case Some(a) => a
      case None    => I.identity
    }

  /**
   * Lifts a function operating on values to a function that operates on each
   * element of a collection.
   */
  def map[A, B](f: A => B): F[A] => F[B] =
    (fa: F[A]) => Id.unwrap(forEach[Id, A, B](fa)((a: A) => Id(f(a))))

  /**
   * Statefully maps over the elements of the collection, maintaining some
   * state along the way and returning the final state along with the new
   * collection. For each element, `f` receives the current state and returns
   * the next state and the new element.
   */
  def mapAccum[S, A, B](fa: F[A])(s: S)(f: (S, A) => (S, B)): (S, F[B]) =
    forEach[({ type lambda[+A] = State[S, A] })#lambda, A, B](fa)(a => State.modify((s: S) => f(s, a).swap)).run(s)

  /**
   * Returns the largest value in the collection if one exists or `None`
   * otherwise.
   */
  def maxOption[A: Ord](fa: F[A]): Option[A] =
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
   * Returns whether the collection contains at least one element.
   */
  def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  /**
   * Partitions the collection with the specified function, returning the
   * `Left` values and the `Right` values as two collections.
   */
  def partitionMap[A, B, C](
    fa: F[A]
  )(f: A => Either[B, C])(implicit both: IdentityBoth[F], either: IdentityEither[F]): (F[B], F[C]) = {
    implicit val covariant: Covariant[F]       = self
    implicit val leftIdentity: Identity[F[B]]  = Identity.fromIdentityEitherCovariant
    implicit val rightIdentity: Identity[F[C]] = Identity.fromIdentityEitherCovariant
    foldMap(fa) { a =>
      f(a) match {
        case Left(b)  => (b.succeed, either.none)
        case Right(c) => (either.none, c.succeed)
      }
    }
  }

  /**
   * Partitions the collection with the specified validation function,
   * returning every error of the failed validations and the values of the
   * successful ones as two collections. The logs of the validations are
   * discarded.
   */
  def partitionMapV[W, E, A, B](
    fa: F[A]
  )(f: A => ZValidation[W, E, B])(implicit both: IdentityBoth[F], either: IdentityEither[F]): (F[E], F[B]) = {
    implicit val forEach = self
    ZValidation.partition(fa)(f).get
  }

  /**
   * Partitions the collection with the specified effectual function,
   * returning the `Left` values and the `Right` values as two collections
   * inside the effect.
   */
  def partitionMapM[G[+_]: IdentityBoth: Covariant, A, B, C](
    fa: F[A]
  )(f: A => G[Either[B, C]])(implicit both: IdentityBoth[F], either: IdentityEither[F]): G[(F[B], F[C])] = {
    implicit val covariant: Covariant[F]       = self
    implicit val leftIdentity: Identity[F[B]]  = Identity.fromIdentityEitherCovariant
    implicit val rightIdentity: Identity[F[C]] = Identity.fromIdentityEitherCovariant
    foldMapM(fa) { a =>
      f(a).map {
        case Left(b)  => (b.succeed[F], either.none)
        case Right(c) => (either.none, c.succeed[F])
      }
    }
  }

  /**
   * Returns the product of the elements in the collection, using the
   * `Identity` instance of `Prod[A]`; an empty collection gives the identity
   * (`1` for numbers).
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
   * returning `None` if the collection is empty. The `Equal` instance lets the
   * operation skip combining values that are equal.
   */
  def reduceIdempotent[A: Idempotent: Equal](fa: F[A]): Option[A] =
    reduceAssociative(fa)(Idempotent[A].idempotent)

  /**
   * Reduces the collection to a summary value using the `Identity` instance
   * of `A`, or returns the identity element if the collection is empty. Alias
   * for `fold`.
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
   * Reduces the collection from left to right using the binary function `f`,
   * returning `None` if the collection is empty.
   */
  def reduceOption[A](fa: F[A])(f: (A, A) => A): Option[A] =
    foldLeft[Option[A], A](fa)(None) {
      case (Some(s), a) => Some(f(s, a))
      case (None, a)    => Some(a)
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
   * Returns the sum of the elements in the collection, using the `Identity`
   * instance of `Sum[A]`; an empty collection gives the identity (`0` for
   * numbers).
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
   * Zips the left collection and right collection together. Pairs become
   * `These.Both`, and the extra elements of the longer collection become
   * `These.Left` or `These.Right`.
   */
  def zipAll[A, B, C](fa: F[A], fb: F[B])(implicit
    both: IdentityBoth[F],
    either: IdentityEither[F]
  ): F[These[A, B]] =
    zipAllWith(fa, fb)(identity)

  /**
   * Zips the left collection and right collection together and applies `f`
   * to each result: `These.Both` for a pair, and `These.Left` or `These.Right`
   * for the extra elements of the longer collection.
   */
  def zipAllWith[A, B, C](fa: F[A], fb: F[B])(
    f: These[A, B] => C
  )(implicit both: IdentityBoth[F], either: IdentityEither[F]): F[C] = {
    implicit val covariant: Covariant[F] = self
    val as                               = toChunk(fa)
    val bs                               = toChunk(fb)
    val cs                               = as.zipAllWith(bs)(a => f(These.Left(a)), b => f(These.Right(b)))((a, b) => f(These.Both(a, b)))
    cs.foldLeft[F[C]](either.none)((cs, c) => cs orElse c.succeed)
  }

  /**
   * Zips each element of the collection with its index, starting from `0`.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    forEach[({ type lambda[+A] = State[Int, A] })#lambda, A, (A, Int)](fa)(a =>
      State.modify((n: Int) => ((a, n), n + 1))
    ).runResult(0)

  /**
   * Composes this `ForEach` with a `ForEach` for `G`, giving a `ForEach` for
   * the nested type `F[G[A]]` that traverses every inner element.
   */
  final def compose[G[+_]: ForEach]: ForEach[({ type lambda[+A] = F[G[A]] })#lambda] =
    new ForEach[({ type lambda[+A] = F[G[A]] })#lambda] {
      def forEach[H[+_]: IdentityBoth: Covariant, A, B](fa: F[G[A]])(f: A => H[B]) =
        self.forEach(fa)(_.forEach(f))
    }
}

/**
 * Summons `ForEach` instances.
 */
object ForEach {

  /**
   * Summons an implicit `ForEach`.
   */
  def apply[F[+_]](implicit forEach: ForEach[F]): ForEach[F] =
    forEach
}

/**
 * Infix syntax for `ForEach`. The `zio.prelude` package object mixes this
 * trait in, so `import zio.prelude._` brings it into scope.
 */
trait ForEachSyntax {

  /**
   * Provides infix syntax for traversing collections. Each method calls the
   * `ForEach` method of the same name. When the collection type has its own
   * method with that name, such as `List#filter`, Scala calls that method
   * instead.
   */
  implicit class ForEachOps[F[+_], A](private val self: F[A]) {

    /** Collects the results of `pf` for the elements on which it is defined. */
    def collect[B](pf: PartialFunction[A, B])(implicit
      F: ForEach[F],
      I: IdentityEither[F],
      B: IdentityBoth[F]
    ): F[B] =
      F.collect(self)(pf)

    /** Runs `f` on each element and collects the values it returns in `Some`. */
    def collectM[G[+_]: IdentityBoth: Covariant, B](f: A => G[Option[B]])(implicit
      F: ForEach[F],
      I: IdentityEither[F],
      B: IdentityBoth[F]
    ): G[F[B]] =
      F.collectM(self)(f)

    /** Combines the elements with their `Identity` instance. */
    def concatenate(implicit F: ForEach[F], A: Identity[A]): A =
      F.concatenate(self)

    /** Traverses the collection with the effectual function `f`. */
    def forEach[G[+_]: IdentityBoth: Covariant, B](f: A => G[B])(implicit F: ForEach[F]): G[F[B]] =
      F.forEach(self)(f)

    /** Returns whether the collection contains `a`, compared with `Equal`. */
    def contains[A1 >: A](a: A1)(implicit A: Equal[A1], F: ForEach[F]): Boolean =
      F.contains[A, A1](self)(a)

    /** Returns the number of elements that satisfy `f`. */
    def count(f: A => Boolean)(implicit F: ForEach[F]): Int =
      F.count(self)(f)

    /** Returns whether any element satisfies `f`. */
    def exists(f: A => Boolean)(implicit F: ForEach[F]): Boolean =
      F.exists(self)(f)

    /** Keeps the elements that satisfy `f`. */
    def filter(f: A => Boolean)(implicit F: ForEach[F], I: IdentityEither[F], B: IdentityBoth[F]): F[A] =
      F.filter(self)(f)

    /** Keeps the elements for which the effectual predicate `f` returns `true`. */
    def filterM[G[+_]: IdentityBoth: Covariant](f: A => G[Boolean])(implicit
      F: ForEach[F],
      I: IdentityEither[F],
      B: IdentityBoth[F]
    ): G[F[A]] =
      F.filterM(self)(f)

    /** Returns the first element that satisfies `f`, or `None`. */
    def find(f: A => Boolean)(implicit F: ForEach[F]): Option[A] =
      F.find(self)(f)

    /** Combines the elements with their `Identity` instance. */
    def fold(implicit F: ForEach[F], I: Identity[A]): A =
      F.fold(self)

    /** Folds over the elements from left to right, starting from `s`. */
    def foldLeft[S](s: S)(f: (S, A) => S)(implicit F: ForEach[F]): S =
      F.foldLeft(self)(s)(f)

    /** Folds over the elements from left to right with the effectual function `f`. */
    def foldLeftM[G[+_]: IdentityFlatten: Covariant, S](s: S)(f: (S, A) => G[S])(implicit F: ForEach[F]): G[S] =
      F.foldLeftM(self)(s)(f)

    /** Maps each element with `f` and combines the results with the `Identity` instance of `B`. */
    def foldMap[B: Identity](f: A => B)(implicit F: ForEach[F]): B =
      F.foldMap(self)(f)

    /** Maps each element with the effectual function `f` and combines the results with the `Identity` instance of `B`. */
    def foldMapM[G[+_]: Covariant: IdentityBoth, B: Identity](f: A => G[B])(implicit F: ForEach[F]): G[B] =
      F.foldMapM(self)(f)

    /** Folds over the elements from right to left, starting from `s`. */
    def foldRight[S](s: S)(f: (A, S) => S)(implicit F: ForEach[F]): S =
      F.foldRight(self)(s)(f)

    /** Folds over the elements from right to left with the effectual function `f`. */
    def foldRightM[G[+_]: IdentityFlatten: Covariant, S](s: S)(f: (A, S) => G[S])(implicit F: ForEach[F]): G[S] =
      F.foldRightM(self)(s)(f)

    /** Returns whether every element satisfies `f`. */
    def forall(f: A => Boolean)(implicit F: ForEach[F]): Boolean =
      F.forall(self)(f)

    /** Traverses the collection with `f` for its effects only. */
    def forEach_[G[+_]: IdentityBoth: Covariant](f: A => G[Any])(implicit F: ForEach[F]): G[Unit] =
      F.forEach_(self)(f)

    /** Traverses the collection with `f` and flattens the collections it returns. */
    def forEachFlatten[G[+_]: IdentityBoth: Covariant, B](
      f: A => G[F[B]]
    )(implicit F: ForEach[F], AF: AssociativeFlatten[F]): G[F[B]] =
      F.forEachFlatten(self)(f)

    /** Groups the elements by the key that `f` returns. */
    def groupByNonEmpty[K](f: A => K)(implicit F: ForEach[F]): Map[K, NonEmptyChunk[A]] =
      F.groupByNonEmpty(self)(f)

    /** Groups the elements by the key that the effectual function `f` returns. */
    def groupByNonEmptyM[G[+_]: IdentityBoth: Covariant, K](f: A => G[K])(implicit
      F: ForEach[F]
    ): G[Map[K, NonEmptyChunk[A]]] =
      F.groupByNonEmptyM(self)(f)

    /** Returns whether the collection is empty. */
    def isEmpty(implicit F: ForEach[F]): Boolean =
      F.isEmpty(self)

    /** Combines the elements with `middle` in between each pair. */
    def intersperse[A1 >: A](middle: A1)(implicit F: ForEach[F], I: Identity[A1]): A1 =
      F.intersperse(self, middle)

    /** Maps over the elements while threading the state `s`, returning the final state and the new collection. */
    def mapAccum[S, B](s: S)(f: (S, A) => (S, B))(implicit F: ForEach[F]): (S, F[B]) =
      F.mapAccum(self)(s)(f)

    /** Returns the largest element, or `None` if the collection is empty. */
    def maxOption(implicit A: Ord[A], F: ForEach[F]): Option[A] =
      F.maxOption(self)

    /** Returns the element with the largest `f` value, or `None` if the collection is empty. */
    def maxByOption[B: Ord](f: A => B)(implicit F: ForEach[F]): Option[A] =
      F.maxByOption(self)(f)

    /** Returns the smallest element, or `None` if the collection is empty. */
    def minOption(implicit A: Ord[A], F: ForEach[F]): Option[A] =
      F.minOption(self)

    /** Returns the element with the smallest `f` value, or `None` if the collection is empty. */
    def minByOption[B: Ord](f: A => B)(implicit F: ForEach[F]): Option[A] =
      F.minByOption(self)(f)

    /** Returns whether the collection contains at least one element. */
    def nonEmpty(implicit F: ForEach[F]): Boolean =
      F.nonEmpty(self)

    /** Combines the elements with their `Associative` instance, or returns `None` if the collection is empty. */
    def reduceAssociative(implicit F: ForEach[F], A: Associative[A]): Option[A] =
      F.reduceAssociative(self)

    /** Combines the elements with their `Idempotent` instance, or returns `None` if the collection is empty. */
    def reduceIdempotent(implicit F: ForEach[F], ia: Idempotent[A], ea: Equal[A]): Option[A] =
      F.reduceIdempotent(self)

    /** Combines the elements with their `Identity` instance. */
    def reduceIdentity(implicit F: ForEach[F], A: Identity[A]): A =
      F.reduceIdentity(self)

    /** Splits the elements into the `Left` and the `Right` results of `f`. */
    def partitionMap[B, C](
      f: A => Either[B, C]
    )(implicit F: ForEach[F], both: IdentityBoth[F], either: IdentityEither[F]): (F[B], F[C]) =
      F.partitionMap(self)(f)

    /** Splits the elements into the errors and the values of the validations that `f` returns. */
    def partitionMapV[W, E, B](
      f: A => ZValidation[W, E, B]
    )(implicit F: ForEach[F], both: IdentityBoth[F], either: IdentityEither[F]): (F[E], F[B]) =
      F.partitionMapV(self)(f)

    /** Splits the elements into the `Left` and the `Right` results of the effectual function `f`. */
    def partitionMapM[G[+_]: IdentityBoth: Covariant, B, C](
      f: A => G[Either[B, C]]
    )(implicit F: ForEach[F], both: IdentityBoth[F], either: IdentityEither[F]): G[(F[B], F[C])] =
      F.partitionMapM(self)(f)

    /** Returns the product of the elements. */
    def product(implicit A: Identity[Prod[A]], F: ForEach[F]): A =
      F.product(self)

    /** Maps each element with `f` and combines the results, or returns `None` if the collection is empty. */
    def reduceMapOption[B: Associative](f: A => B)(implicit F: ForEach[F]): Option[B] =
      F.reduceMapOption(self)(f)

    /** Reduces the elements from left to right with `f`, or returns `None` if the collection is empty. */
    def reduceOption(f: (A, A) => A)(implicit F: ForEach[F]): Option[A] =
      F.reduceOption(self)(f)

    /** Reverses the order of the elements. */
    def reverse(implicit F: ForEach[F]): F[A] =
      F.reverse(self)

    /** Returns the number of elements. */
    def size(implicit F: ForEach[F]): Int =
      F.size(self)

    /** Returns the sum of the elements. */
    def sum(implicit A: Identity[Sum[A]], F: ForEach[F]): A =
      F.sum(self)

    /** Converts the collection to a `Chunk`. */
    def toChunk(implicit F: ForEach[F]): Chunk[A] =
      F.toChunk(self)

    /** Keeps the elements that satisfy `f`; lets a for comprehension use an `if` guard. */
    def withFilter(f: A => Boolean)(implicit F: ForEach[F], I: IdentityEither[F], B: IdentityBoth[F]): F[A] =
      F.filter(self)(f)

    /** Zips with `that`, keeping the extra elements of the longer collection as `These.Left` or `These.Right`. */
    def zipAll[B](
      that: F[B]
    )(implicit F: ForEach[F], both: IdentityBoth[F], either: IdentityEither[F]): F[These[A, B]] =
      F.zipAll(self, that)

    /** Zips with `that` and applies `f` to each `These` value. */
    def zipAllWith[B, C](that: F[B])(
      f: These[A, B] => C
    )(implicit F: ForEach[F], both: IdentityBoth[F], either: IdentityEither[F]): F[C] =
      F.zipAllWith(self, that)(f)

    /** Zips each element with its index, starting from `0`. */
    def zipWithIndex(implicit F: ForEach[F]): F[(A, Int)] =
      F.zipWithIndex(self)
  }

  /**
   * Provides infix syntax for flip.
   */
  implicit class FlipOps[F[+_], G[+_], A](private val self: F[G[A]]) {

    /**
     * Turns a collection of effects into an effect of a collection, for
     * example a `List[Option[A]]` into an `Option[List[A]]`.
     */
    def flip[B](implicit forEach: ForEach[F], identityBoth: IdentityBoth[G], covariant: Covariant[G]): G[F[A]] =
      forEach.flip(self)
  }
}
