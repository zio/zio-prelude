package zio.prelude

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.{ Success, Try }

import zio._
import zio.prelude.coherent.AssociativeFlattenCovariantDeriveEqual
import zio.stream.ZStream
import zio.test.TestResult
import zio.test.laws._

/**
 * `AssociativeFlatten` describes a type that can be "flattened" in an
 * associative way. For example, if we have a list of lists of lists, we can
 * flatten it by either flattening the two inner lists and then flattening the
 * resulting lists, or flattening the two outer lists and then flattening that
 * resulting list. Because the operation is associative, the resulting list is
 * the same either way.
 */
@implicitNotFound("No implicit AssociativeFlatten defined for ${F}.")
trait AssociativeFlatten[F[+_]] {

  /**
   * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
   */
  def flatten[A](ffa: F[F[A]]): F[A]
}

object AssociativeFlatten extends LawfulF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] {

  /**
   * For all `fffa`, `flatten(flatten(fffa))` is equivalent to
   * `flatten(fffa.map(flatten))`.
   */
  val associativityLaw: LawsF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] =
    new LawsF.Covariant.FlattenLaw[AssociativeFlattenCovariantDeriveEqual, Equal]("associativityLaw") {
      def apply[F[+_]: AssociativeFlattenCovariantDeriveEqual, A: Equal](fffa: F[F[F[A]]]): TestResult =
        fffa.flatten.flatten <-> fffa.map(_.flatten).flatten
    }

  /**
   * The set of all laws that instances of `AssociativeFlatten` must satisfy.
   */
  val laws: LawsF.Covariant[AssociativeFlattenCovariantDeriveEqual, Equal] =
    associativityLaw

  /**
   * Summons an implicit `AssociativeFlatten[F]`.
   */
  def apply[F[+_]](implicit associativeFlatten: AssociativeFlatten[F]): AssociativeFlatten[F] =
    associativeFlatten

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Cause`.
   */
  implicit val CauseIdentityFlatten: IdentityFlatten[Cause] =
    new IdentityFlatten[Cause] {
      override def any: Cause[Any] = Cause.fail(())

      override def flatten[A](ffa: Cause[Cause[A]]): Cause[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Chunk`.
   */
  implicit val ChunkIdentityFlatten: IdentityFlatten[Chunk] =
    new IdentityFlatten[Chunk] {
      def any: Chunk[Any] = Chunk.unit

      def flatten[A](ffa: Chunk[Chunk[A]]): Chunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Either`.
   */
  implicit def EitherIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] {
      def any: Either[E, Any] = Right(())

      def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] =
        ffa match {
          case Left(e)         => Left(e)
          case Right(Left(e))  => Left(e)
          case Right(Right(a)) => Right(a)
        }
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Exit`.
   */
  implicit def ExitIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def any: Exit[E, Any] = Exit.unit

      def flatten[A](ffa: Exit[E, Exit[E, A]]): Exit[E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Future`.
   */
  implicit val FutureIdentityFlatten: IdentityFlatten[Future] =
    new IdentityFlatten[Future] {
      def any: Future[Any] = Future.successful(())

      def flatten[A](ffa: Future[Future[A]]): Future[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `List`.
   */
  implicit val ListIdentityFlatten: IdentityFlatten[List] =
    new IdentityFlatten[List] {
      def any: List[Any] = List(())

      def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Map`
   */
  implicit def MapAssociativeFlatten[K]: AssociativeFlatten[({ type lambda[+v] = Map[K, v] })#lambda] =
    new AssociativeFlatten[({ type lambda[+v] = Map[K, v] })#lambda] {
      def flatten[V](ffa: Map[K, Map[K, V]]): Map[K, V] =
        ffa.foldLeft[Map[K, V]](Map.empty) { case (l, (_, r)) => r.foldLeft(l)(_ + _) }
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkIdentityFlatten: IdentityFlatten[NonEmptyChunk] =
    new IdentityFlatten[NonEmptyChunk] {
      def any: NonEmptyChunk[Any] = NonEmptyChunk.single(())

      def flatten[A](ffa: NonEmptyChunk[NonEmptyChunk[A]]): NonEmptyChunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Option`.
   */
  implicit val OptionIdentityFlatten: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any] = Some(())

      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Try`.
   */
  implicit val TryIdentityFlatten: IdentityFlatten[Try] =
    new IdentityFlatten[Try] {
      def any: Try[Any] = Success(())

      def flatten[A](ffa: Try[Try[A]]): Try[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `Vector`.
   */
  implicit val VectorIdentityFlatten: IdentityFlatten[Vector] =
    new IdentityFlatten[Vector] {
      def any: Vector[Any] = Vector(())

      def flatten[A](ffa: Vector[Vector[A]]): Vector[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZIO`.
   */
  implicit def ZIOIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def any: ZIO[R, E, Any] = ZIO.unit

      def flatten[A](ffa: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZManaged`.
   */
  implicit def ZManagedIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def any: ZManaged[R, E, Any] = ZManaged.unit

      def flatten[A](ffa: ZManaged[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` and `IdentityFlatten` instance for `ZStream`.
   */
  implicit def ZStreamIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def any: ZStream[R, E, Any] = ZStream.unit

      def flatten[A](ffa: ZStream[R, E, ZStream[R, E, A]]): ZStream[R, E, A] = ffa.flatten
    }

}

trait AssociativeFlattenSyntax {

  /**
   * Provides infix syntax for flattening types.
   */
  implicit class AssociativeFlattenOps[F[+_], A](ffa: F[F[A]]) {

    /**
     * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
     */
    def flatten(implicit flatten: AssociativeFlatten[F]): F[A] =
      flatten.flatten(ffa)
  }

  /**
   * Provides infix syntax for flattening covariant types.
   */
  implicit class AssociativeFlattenCovariantOps[F[+_], A](fa: F[A]) {

    /**
     * Maps a function `A => F[B]` over an `F[A]` value and then flattens the
     * resulting `F[F[B]]`.
     */
    def flatMap[B](f: A => F[B])(implicit flatten: AssociativeFlatten[F], covariant: Covariant[F]): F[B] =
      flatten.flatten(covariant.map(f)(fa))
  }
}
