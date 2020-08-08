package zio.prelude

import zio.{ Chunk, ChunkBuilder }
import zio.prelude.coherent.DeriveEqualTraversable
import zio.prelude.newtypes.{ And, First, Max, Min, Or, Prod, Sum }
import zio.test.laws._

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
  def foreach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Returns whether the collection contains the specified element.
   */
  def contains[A, A1 >: A](fa: F[A])(a: A1)(implicit A: Equal[A1]): Boolean =
    exists(fa)(_ === a)

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
   * Converts a collection with elements that are in the context of effects to
   * a collection of elements in the context of an effect.
   */
  def flip[G[+_]: IdentityBoth: Covariant, A](fa: F[G[A]]): G[F[A]] =
    foreach(fa)(identity(_))

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
  def foldLeft[S, A](fa: F[A])(s: S)(f: (S, A) => S): S =
    foreach(fa)(a => State.update(f(_, a))).runState(s)

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
   * Returns whether any element of the collection satisfies the specified
   * predicate.
   */
  def forall[A](fa: F[A])(f: A => Boolean): Boolean =
    foldMap(fa)(a => And(f(a)))

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  def foreach_[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    foreach(fa)(f).as(())

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
    (fa: F[A]) => Id.unwrap(foreach[Id, A, B](fa)((a: A) => Id(f(a))))

  /**
   * Statefully maps over the elements of the collection, maintaining some
   * state along the way and returning the final state along with the new
   * collection.
   */
  def mapAccum[S, A, B](fa: F[A])(s: S)(f: (S, A) => (S, B)): (S, F[B]) =
    foreach(fa)(a => State.modify(f(_, a))).run(s)

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
    foreach(fa)(a => State.modify((n: Int) => (n + 1, (a, n)))).runResult(0)
}

object Traversable extends LawfulF.Covariant[DeriveEqualTraversable, Equal] with TraversableVersionSpecific {

  /**
   * The set of all laws that instances of `Traversable` must satisfy.
   */
  val laws: LawsF.Covariant[DeriveEqualTraversable, Equal] =
    Covariant.laws

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
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](chunk: Chunk[A])(f: A => G[B]): G[Chunk[B]] =
        chunk.foldLeft(ChunkBuilder.make[B]().succeed)((builder, a) => builder.zipWith(f(a))(_ += _)).map(_.result())
    }

  /**
   * The `Traversable` instance for `Either`.
   */
  implicit def EitherTraversable[E]: Traversable[({ type lambda[+a] = Either[E, a] })#lambda] =
    new Traversable[({ type lambda[+a] = Either[E, a] })#lambda] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](either: Either[E, A])(f: A => G[B]): G[Either[E, B]] =
        either.fold(Left(_).succeed, f(_).map(Right(_)))
    }

  /**
   * The `Traversable` instance for `List`.
   */
  implicit val ListTraversable: Traversable[List] =
    new Traversable[List] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]] =
        list.foldRight[G[List[B]]](Nil.succeed)((a, bs) => f(a).zipWith(bs)(_ :: _))
      override def map[A, B](f: A => B): List[A] => List[B] = _.map(f)
    }

  /**
   * The `Traversable` instance for `Map`.
   */
  implicit def MapTraversable[K]: Traversable[({ type lambda[+v] = Map[K, v] })#lambda] =
    new Traversable[({ type lambda[+v] = Map[K, v] })#lambda] {
      def foreach[G[+_]: IdentityBoth: Covariant, V, V2](map: Map[K, V])(f: V => G[V2]): G[Map[K, V2]] =
        map.foldLeft[G[Map[K, V2]]](Map.empty.succeed) {
          case (map, (k, v)) =>
            map.zipWith(f(v))((map, v2) => map + (k -> v2))
        }
    }

  /**
   * The `Traversable` instance for `Option`.
   */
  implicit val OptionTraversable: Traversable[Option] =
    new Traversable[Option] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](option: Option[A])(f: A => G[B]): G[Option[B]] =
        option.fold[G[Option[B]]](Option.empty.succeed)(a => f(a).map(Some(_)))
    }

  /**
   * The `Traversable` instance for `Vector`.
   */
  implicit val VectorTraversable: Traversable[Vector] =
    new Traversable[Vector] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](vector: Vector[A])(f: A => G[B]): G[Vector[B]] =
        vector.foldLeft[G[Vector[B]]](Vector.empty.succeed)((bs, a) => bs.zipWith(f(a))(_ :+ _))
    }
}

trait TraversableSyntax {

  /**
   * Provides infix syntax for traversing collections.
   */
  implicit class TraversableOps[F[+_], A](private val self: F[A]) {
    def foreach[G[+_]: IdentityBoth: Covariant, B](f: A => G[B])(implicit F: Traversable[F]): G[F[B]] =
      F.foreach(self)(f)
    def contains[A1 >: A](a: A1)(implicit A: Equal[A1], F: Traversable[F]): Boolean =
      F.contains[A, A1](self)(a)
    def count(f: A => Boolean)(implicit F: Traversable[F]): Int =
      F.count(self)(f)
    def exists(f: A => Boolean)(implicit F: Traversable[F]): Boolean =
      F.exists(self)(f)
    def find(f: A => Boolean)(implicit F: Traversable[F]): Option[A] =
      F.find(self)(f)
    def foldLeft[S](s: S)(f: (S, A) => S)(implicit F: Traversable[F]): S =
      F.foldLeft(self)(s)(f)
    def foldMap[B: Identity](f: A => B)(implicit F: Traversable[F]): B =
      F.foldMap(self)(f)
    def foldRight[S](s: S)(f: (A, S) => S)(implicit F: Traversable[F]): S =
      F.foldRight(self)(s)(f)
    def forall(f: A => Boolean)(implicit F: Traversable[F]): Boolean =
      F.forall(self)(f)
    def foreach_[G[+_]: IdentityBoth: Covariant](f: A => G[Any])(implicit F: Traversable[F]): G[Unit] =
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
    def reverse(implicit F: Traversable[F]): F[A] =
      F.reverse(self)
    def size(implicit F: Traversable[F]): Int =
      F.size(self)
    def sum(implicit A: Identity[Sum[A]], F: Traversable[F]): A =
      F.sum(self)
    def toChunk(implicit F: Traversable[F]): Chunk[A] =
      F.toChunk(self)
    def zipWithIndex(implicit F: Traversable[F]): F[(A, Int)] =
      F.zipWithIndex(self)
  }

  /**
   * Provides infix syntax for flip.
   */
  implicit class FlipOps[F[+_], G[+_], A](private val self: F[G[A]]) {
    def flip[B](implicit traversable: Traversable[F], identityBoth: IdentityBoth[G], covariant: Covariant[G]): G[F[A]] =
      traversable.flip(self)
  }
}
