package zio.prelude

import zio._
import zio.prelude.coherent.CovariantDeriveEqualIdentityFlatten
import zio.stream.ZStream
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.{ Success, Try }

/**
 * `IdentityFlatten` described a type that can be "flattened" in an
 * associative way and has an identity element with respect to that operation.
 * For example, with a list we can always vacuously add a layer by wrapping a
 * list in another list constructor and flattening the resulting list always
 * returns the original list unchanged.
 */
@implicitNotFound("No implicit IdentityFlatten defined for ${F}.")
trait IdentityFlatten[F[+_]] extends AssociativeFlatten[F] { self =>

  /**
   * The identity element.
   */
  def any: F[Any]
}

object IdentityFlatten extends LawfulF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] {

  /**
   * Adding a layer by mapping a value and mapping it into the identity
   * element and then flattening is an identity.
   */
  val rightIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("rightIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        fa.map(a => IdentityFlatten[F].any.map(_ => a)).flatten <-> fa
    }

  /**
   * Adding a layer by mapping a value into the identity element and then
   * flattening is an identity
   */
  val leftIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("leftIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        IdentityFlatten[F].any.map(_ => fa).flatten <-> fa
    }

  /**
   * The set of all laws that instances of `IdentityFlatten` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    rightIdentityLaw + leftIdentityLaw + AssociativeFlatten.laws

  /**
   * Summons an implicit `IdentityFlatten[F]`.
   */
  def apply[F[+_]](implicit identityFlatten: IdentityFlatten[F]): IdentityFlatten[F] =
    identityFlatten

  /**
   * The `IdentityFlatten` instance for `Cause`.
   */
  implicit val CauseIdentityFlatten: IdentityFlatten[Cause] =
    new IdentityFlatten[Cause] {
      override def any: Cause[Any] = Cause.fail(())

      override def flatten[A](ffa: Cause[Cause[A]]): Cause[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Chunk`.
   */
  implicit val ChunkIdentityFlatten: IdentityFlatten[Chunk] =
    new IdentityFlatten[Chunk] {
      def any: Chunk[Any] = Chunk.unit

      def flatten[A](ffa: Chunk[Chunk[A]]): Chunk[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Either`.
   */
  implicit def EitherIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Either[E, a] })#lambda] {
      def any: Either[E, Any] = Right(())

      def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] = ffa match {
        case Left(e)         => Left(e)
        case Right(Left(e))  => Left(e)
        case Right(Right(a)) => Right(a)
      }
    }

  /**
   * The `IdentityFlatten` instance for `Exit`.
   */
  implicit def ExitIdentityFlatten[E]: IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def any: Exit[E, Any] = Exit.unit

      def flatten[A](ffa: Exit[E, Exit[E, A]]): Exit[E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Future`.
   */
  implicit val FutureIdentityFlatten: IdentityFlatten[Future] =
    new IdentityFlatten[Future] {
      def any: Future[Any] = Future.successful(())

      def flatten[A](ffa: Future[Future[A]]): Future[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Id`.
   */
  implicit val IdIdentityFlatten: IdentityFlatten[Id] =
    new IdentityFlatten[Id] {
      def any: Id[Any] = Id(())

      def flatten[A](ffa: Id[Id[A]]): Id[A] = Id.unwrap(ffa)
    }

  /**
   * The `IdentityFlatten` instance for `List`.
   */
  implicit val ListIdentityFlatten: IdentityFlatten[List] =
    new IdentityFlatten[List] {
      def any: List[Any] = List(())

      def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkIdentityFlatten: IdentityFlatten[NonEmptyChunk] =
    new IdentityFlatten[NonEmptyChunk] {
      def any: NonEmptyChunk[Any] = NonEmptyChunk.single(())

      def flatten[A](ffa: NonEmptyChunk[NonEmptyChunk[A]]): NonEmptyChunk[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Option`.
   */
  implicit val OptionIdentityFlatten: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any]                              = Some(())
      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Try`.
   */
  implicit val TryIdentityFlatten: IdentityFlatten[Try] =
    new IdentityFlatten[Try] {
      def any: Try[Any]                        = Success(())
      def flatten[A](ffa: Try[Try[A]]): Try[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Vector`.
   */
  implicit val VectorIdentityFlatten: IdentityFlatten[Vector] =
    new IdentityFlatten[Vector] {
      def any: Vector[Any] = Vector(())

      def flatten[A](ffa: Vector[Vector[A]]): Vector[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZIO`.
   */
  implicit def ZIOIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def any: ZIO[R, E, Any] = ZIO.unit

      def flatten[A](ffa: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZManaged`.
   */
  implicit def ZManagedIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def any: ZManaged[R, E, Any] = ZManaged.unit

      def flatten[A](ffa: ZManaged[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZStream`.
   */
  implicit def ZStreamIdentityFlatten[R, E]: IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def any: ZStream[R, E, Any] = ZStream.unit

      def flatten[A](ffa: ZStream[R, E, ZStream[R, E, A]]): ZStream[R, E, A] = ffa.flatten
    }

}
