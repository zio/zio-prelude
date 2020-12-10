package zio.prelude

import zio.prelude.coherent.DeriveEqualNonEmptyTraversable
import zio.prelude.newtypes.{ Max, Min }
import zio.test.laws._
import zio.{ ChunkBuilder, NonEmptyChunk }

/**
 * A `NonEmptyTraversable` describes a `Traversable` that is guaranteed to
 * contain at least one element, such as a `NonEmptyList`, a `NonEmptyChunk`,
 * or certain tree like data structures.
 *
 * Because of the additional information that there is always at least one
 * element, certain operations are available on a `NonEmptyTraversable` that
 * are not available on a `Traversable`. For example, if an ordering is
 * defined on the elements of a `NonEmptyTraversable` then `min` and `max` are
 * defined, whereas for a `Traversable` only `minOption` and `maxOption` would
 * be, since the collection might not contain any elements at all.
 */
trait NonEmptyTraversable[F[+_]] extends Traversable[F] {

  /**
   * Traverse each element in the collection using the specified effectual
   * function `f`, returning a new collection with the results in the context
   * of the effect.
   */
  def foreach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Converts a collection with elements that are in the context of effects to
   * a collection of elements in the context of an effect.
   */
  def flip1[G[+_]: AssociativeBoth: Covariant, A](fa: F[G[A]]): G[F[A]] =
    foreach1(fa)(identity)

  override def foreach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    foreach1(fa)(f)

  /**
   * Traverses each element in the collection with the specified effectual
   * function `f` purely for its effects.
   */
  def foreach1_[G[+_]: AssociativeBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    foreach1(fa)(f).as(())

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
  def reduce[A](fa: F[A])(f: (A, A) => A): A = {
    implicit val associative: Associative[A] = Associative.make(f)
    reduceMap(fa)(identity)
  }

  /**
   * Reduces the non-empty collection of associative elements.
   */
  def reduce1[A: Associative](fa: F[A]): A =
    reduceMap(fa)(identity)

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
   * function `reduce` to combine the `B` value with each other `A` value.
   */
  def reduceMapLeft[A, B](fa: F[A])(map: A => B)(reduce: (B, A) => B): B = {
    type StateB[+A] = State[Option[B], A]
    foreach[StateB, A, Any](fa) { a =>
      State.update {
        case None    => Some(map(a))
        case Some(b) => Some(reduce(b, a))
      }
    }.runState(None).get
  }

  /**
   * Reduces the elements of this colection from right to left using the
   * function `map` to transform the first value to the type `B` and then the
   * function `reduce` to combine the `B` value with each other `A` value.
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
object NonEmptyTraversable extends LawfulF.Covariant[DeriveEqualNonEmptyTraversable, Equal] {

  /**
   * The set of all laws that instances of `NonEmptyTraversable` must satisfy.
   */
  val laws: LawsF.Covariant[DeriveEqualNonEmptyTraversable, Equal] =
    Traversable.laws

  /**
   * Summons an implicit `NonEmptyTraversable`.
   */
  def apply[F[+_]](implicit nonEmptyTraversable: NonEmptyTraversable[F]): NonEmptyTraversable[F] =
    nonEmptyTraversable
}

trait NonEmptyTraversableSyntax {

  /**
   * Provides infix syntax for traversing collections.
   */
  implicit class NonEmptyTraversableOps[F[+_], A](private val self: F[A]) {
    def foreach1[G[+_]: AssociativeBoth: Covariant, B](f: A => G[B])(implicit F: NonEmptyTraversable[F]): G[F[B]] =
      F.foreach1(self)(f)
    def foreach1_[G[+_]: AssociativeBoth: Covariant](f: A => G[Any])(implicit F: NonEmptyTraversable[F]): G[Unit] =
      F.foreach1_(self)(f)
    def reduce(f: (A, A) => A)(implicit F: NonEmptyTraversable[F]): A                                             =
      F.reduce(self)(f)
    def reduce1(implicit F: NonEmptyTraversable[F], A: Associative[A]): A                                         =
      F.reduce1(self)
    def reduceMap[B: Associative](f: A => B)(implicit F: NonEmptyTraversable[F]): B                               =
      F.reduceMap(self)(f)
    def reduceMapLeft[B](map: A => B)(reduce: (B, A) => B)(implicit F: NonEmptyTraversable[F]): B                 =
      F.reduceMapLeft(self)(map)(reduce)
    def reduceMapRight[B](map: A => B)(reduce: (A, B) => B)(implicit F: NonEmptyTraversable[F]): B                =
      F.reduceMapRight(self)(map)(reduce)
    def toNonEmptyChunk(implicit F: NonEmptyTraversable[F]): NonEmptyChunk[A]                                     =
      F.toNonEmptyChunk(self)
    def toNonEmptyList(implicit F: NonEmptyTraversable[F]): NonEmptyList[A]                                       =
      F.toNonEmptyList(self)
  }

  /**
   * Provides infix syntax for flip1.
   */
  implicit class Flip1Ops[F[+_], G[+_], A](private val self: F[G[A]]) {
    def flip1[B](implicit
      nonEmptyTraversable: NonEmptyTraversable[F],
      associativeBoth: AssociativeBoth[G],
      covariant: Covariant[G]
    ): G[F[A]] =
      nonEmptyTraversable.flip1(self)
  }
}
