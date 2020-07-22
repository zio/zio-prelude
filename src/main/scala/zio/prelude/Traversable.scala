package zio.prelude

import zio.{ Chunk, ChunkBuilder }
import zio.prelude.newtypes.{ And, First, Max, Min, Or, Prod, Sum }

/**
 * `Traversable` is an abstraction that describes the ability to iterate over
 * a collection, performing an effect for each element in the collection and
 * returning a collection with the same shape in the context of the effect.
 *
 * By choosing the appropriate effect type to traverse with a wide range of
 * operations on collections can be described. In particular, by traversing
 * with state we can describe folds which allow implementing a wide variety of
 * collection operations that produce summaries from a collection of values.
 */
trait Traversable[F[+_]] extends Covariant[F] {

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Returns the number of elements in the collection that satisfy the
   * specified prediate.
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
   * Converts a collection of type `F` with elements that are
   */
  def flip[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[G[A]]): G[F[A]] =
    foreach(fa)(identity(_))

  /**
   * Folds over the elements of this collection to produce a summary value,
   * maintaining some internal state along the way.
   */
  def fold[S, A](fa: F[A])(s: S)(f: (S, A) => S): S = {
    type StateS[+A] = State[S, A]
    foreach[StateS, A, Any](fa)((a: A) => State((s: S) => (f(s, a), ()))).runState(s)
  }

  /**
   * Maps each element of the collection to a type `B` for which an `Identity`
   * is defined using the function `f`, then reduces those values to a single
   * summary using the `combine` operation of `Identity`, or the `identity`
   * element if the collection is empty.
   */
  def foldMap[A, B: Identity](fa: F[A])(f: A => B): B =
    fold[B, A](fa)(Identity[B].identity)((b: B, a: A) => b combine f(a))

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
  def foreach_[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    foreach(fa)(f).as(())

  /**
   * Returns whether the collection is empty.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    foldMap(fa)(_ => And(false))

  /**
   * Lifts a function operation on values to a function that operates on each
   * element of a collection.
   */
  def map[A, B](f: A => B): F[A] => F[B] =
    (fa: F[A]) => Id.unwrap(foreach[Id, A, B](fa)((a: A) => Id(f(a))))

  /**
   * Statefully maps over the elements of the collection, maintaining some
   * state along the way and returning the final state along with the new
   * collection.
   */
  def mapAccum[S, A, B](fa: F[A])(s: S)(f: (S, A) => (S, B)): (S, F[B]) = {
    type StateS[+A] = State[S, A]
    foreach[StateS, A, B](fa)(a => State.modify(f(_, a))).run(s)
  }

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
    implicit val associative = Associative.make(f)
    reduceMapOption(fa)(identity)
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
    fold(fa)(ChunkBuilder.make[A]())((builder, a) => builder += a).result()

  /**
   * Zips each element of the collection with its index.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    foreach(fa)(a => State((n: Int) => (n + 1, (a, n)))).runResult(0)
}

object Traversable {

  /**
   * Summons an implicit `Traversable`.
   */
  def apply[F[+_]](implicit traversable: Traversable[F]): Traversable[F] =
    traversable

  /**
   * The `Traversable` instance for `Chunk`.
   */
  implicit val ChunkTraversable: Traversable[Chunk] =
    new Traversable[Chunk] {
      def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](chunk: Chunk[A])(f: A => G[B]): G[Chunk[B]] =
        chunk.foldLeft(ChunkBuilder.make[B]().succeed)((builder, a) => builder.zipWith(f(a))(_ += _)).map(_.result())
    }

  /**
   * The `Traversable` instance for `List`.
   */
  implicit val ListTraversable: Traversable[List] =
    new Traversable[List] {
      def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]] =
        list.foldRight[G[List[B]]](Nil.succeed)((a, bs) => f(a).zipWith(bs)(_ :: _))
      override def map[A, B](f: A => B): List[A] => List[B] = _.map(f)
    }
}

trait TraversableSyntax {

  /**
   * Provides infix syntax for traversing collections.
   */
  implicit class TraversableOps[F[+_], A](private val self: F[A]) {
    def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, B](f: A => G[B])(implicit F: Traversable[F]): G[F[B]] =
      F.foreach(self)(f)
    def count(f: A => Boolean)(implicit F: Traversable[F]): Int =
      F.count(self)(f)
    def exists(f: A => Boolean)(implicit F: Traversable[F]): Boolean =
      F.exists(self)(f)
    def find(f: A => Boolean)(implicit F: Traversable[F]): Option[A] =
      F.find(self)(f)
    def fold[S](s: S)(f: (S, A) => S)(implicit F: Traversable[F]): S =
      F.fold(self)(s)(f)
    def foldMap[B: Identity](f: A => B)(implicit F: Traversable[F]): B =
      F.foldMap(self)(f)
    def forall(f: A => Boolean)(implicit F: Traversable[F]): Boolean =
      F.forall(self)(f)
    def foreach_[G[+_]: AssociativeBoth: IdentityBoth: Covariant](f: A => G[Any])(implicit F: Traversable[F]): G[Unit] =
      F.foreach_(self)(f)
    def isEmpty(implicit F: Traversable[F]): Boolean =
      F.isEmpty(self)
    def mapAccum[S, B](s: S)(f: (S, A) => (S, B))(implicit F: Traversable[F]): (S, F[B]) =
      F.mapAccum(self)(s)(f)
    def maxOption(implicit A: Ord[A], F: Traversable[F]): Option[A] =
      F.maxOption(self)
    def maxByOption[B: Ord](f: A => B)(implicit F: Traversable[F]): Option[A] =
      F.maxByOption(self)(f)
    def minOption(implicit A: Ord[A], F: Traversable[F]): Option[A] =
      F.minOption(self)
    def minByOption[B: Ord](f: A => B)(implicit F: Traversable[F]): Option[A] =
      F.minByOption(self)(f)
    def nonEmpty(implicit F: Traversable[F]): Boolean =
      F.nonEmpty(self)
    def product(implicit A: Identity[Prod[A]], F: Traversable[F]): A =
      F.product(self)
    def reduceMapOption[B: Associative](f: A => B)(implicit F: Traversable[F]): Option[B] =
      F.reduceMapOption(self)(f)
    def reduceOption(f: (A, A) => A)(implicit F: Traversable[F]): Option[A] =
      F.reduceOption(self)(f)
    def size(implicit F: Traversable[F]): Int =
      F.size(self)
    def sum(implicit A: Identity[Sum[A]], F: Traversable[F]): A =
      F.sum(self)
    def toChunk(implicit F: Traversable[F]): Chunk[A] =
      F.toChunk(self)
    def zipWithIndex(implicit F: Traversable[F]): F[(A, Int)] =
      F.zipWithIndex(self)
  }
}
