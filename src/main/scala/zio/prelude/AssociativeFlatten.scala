package zio.prelude

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.Try

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
   * The `AssociativeFlatten` instance for `Cause`.
   */
  implicit val CauseAssociativeFlatten: AssociativeFlatten[Cause] =
    new AssociativeFlatten[Cause] {
      override def flatten[A](ffa: Cause[Cause[A]]): Cause[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Chunk`.
   */
  implicit val ChunkAssociativeFlatten: AssociativeFlatten[Chunk] =
    new AssociativeFlatten[Chunk] {
      def flatten[A](ffa: Chunk[Chunk[A]]): Chunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Either`.
   */
  implicit def EitherAssociativeFlatten[E]: AssociativeFlatten[({ type lambda[+a] = Either[E, a] })#lambda] =
    new AssociativeFlatten[({ type lambda[+a] = Either[E, a] })#lambda] {

      def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] =
        ffa match {
          case Left(e)         => Left(e)
          case Right(Left(e))  => Left(e)
          case Right(Right(a)) => Right(a)
        }
    }

  /**
   * The `AssociativeFlatten` instance for `Exit`.
   */
  implicit def ExitAssociativeFlatten[E]: AssociativeFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new AssociativeFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def flatten[A](ffa: Exit[E, Exit[E, A]]): Exit[E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Future`.
   */
  implicit val FutureAssociativeFlatten: AssociativeFlatten[Future] =
    new AssociativeFlatten[Future] {
      def flatten[A](ffa: Future[Future[A]]): Future[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Id`.
   */
  implicit val IdAssociativeFlatten: AssociativeFlatten[Id] =
    new AssociativeFlatten[Id] {
      def flatten[A](ffa: Id[Id[A]]): Id[A] = Id.unwrap(ffa)
    }

  /**
   * The `AssociativeFlatten` instance for `List`.
   */
  implicit val ListAssociativeFlatten: AssociativeFlatten[List] =
    new AssociativeFlatten[List] {
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
   * The `AssociativeFlatten` instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkAssociativeFlatten: AssociativeFlatten[NonEmptyChunk] =
    new AssociativeFlatten[NonEmptyChunk] {
      def flatten[A](ffa: NonEmptyChunk[NonEmptyChunk[A]]): NonEmptyChunk[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Option`.
   */
  implicit val OptionAssociativeFlatten: AssociativeFlatten[Option] =
    new AssociativeFlatten[Option] {
      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Try`.
   */
  implicit val TryAssociativeFlatten: AssociativeFlatten[Try] =
    new AssociativeFlatten[Try] {
      def flatten[A](ffa: Try[Try[A]]): Try[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `Vector`.
   */
  implicit val VectorAssociativeFlatten: AssociativeFlatten[Vector] =
    new AssociativeFlatten[Vector] {
      def flatten[A](ffa: Vector[Vector[A]]): Vector[A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `ZIO`.
   */
  implicit def ZIOAssociativeFlatten[R, E]: AssociativeFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new AssociativeFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def flatten[A](ffa: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `ZManaged`.
   */
  implicit def ZManagedAssociativeFlatten[R, E]: AssociativeFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new AssociativeFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def flatten[A](ffa: ZManaged[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] = ffa.flatten
    }

  /**
   * The `AssociativeFlatten` instance for `ZStream`.
   */
  implicit def ZStreamAssociativeFlatten[R, E]: AssociativeFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new AssociativeFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def flatten[A](ffa: ZStream[R, E, ZStream[R, E, A]]): ZStream[R, E, A] = ffa.flatten
    }

}

trait AssociativeFlattenSyntax {

  /**
   * Provides infix syntax for flattening covariant types..
   */
  implicit class AssociativeFlattenOps[F[+_], A](ffa: F[F[A]]) {

    /**
     * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
     */
    def flatten(implicit flatten: AssociativeFlatten[F]): F[A] =
      flatten.flatten(ffa)
  }
}
