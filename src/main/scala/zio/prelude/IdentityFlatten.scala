package zio.prelude

import scala.annotation.implicitNotFound

import zio._
import zio.prelude.coherent.CovariantEqualFIdentityFlatten
import zio.stream.ZStream
import zio.test.TestResult
import zio.test.laws._

/**
 * `IdentityFlatten` described a type that can be "flattened" in an
 * associative way and has an identity element with respect to that operation.
 * For example, with a list we can always vacuosly add a layer by wrapping a
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

object IdentityFlatten extends LawfulF.Covariant[CovariantEqualFIdentityFlatten, Equal] {

  /**
   * Adding a layer by mapping a value and mapping it into the identity
   * element and then flattening is an identity.
   */
  val rightIdentityLaw: LawsF.Covariant[CovariantEqualFIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantEqualFIdentityFlatten, Equal]("rightIdentityLaw") {
      def apply[F[+_]: CovariantEqualFIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        fa.map(a => IdentityFlatten[F].any.map(_ => a)).flatten <-> fa
    }

  /**
   * Adding a layer by mapping a value into the identity element and then
   * flattening is an identity
   */
  val leftIdentityLaw: LawsF.Covariant[CovariantEqualFIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantEqualFIdentityFlatten, Equal]("leftIdentityLaw") {
      def apply[F[+_]: CovariantEqualFIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        IdentityFlatten[F].any.map(_ => fa).flatten <-> fa
    }

  /**
   * The set of all laws that instances of `IdentityFlatten` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantEqualFIdentityFlatten, Equal] =
    rightIdentityLaw + leftIdentityLaw + AssociativeFlatten.laws

  /**
   * Summons an implicit `IdentityFlatten[F]`.
   */
  def apply[F[+_]](implicit identityFlatten: IdentityFlatten[F]): IdentityFlatten[F] =
    identityFlatten

  /**
   * The `IdentityFlatten` instance for `Option`.
   */
  implicit val IdentityFlattenOption: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any]                              = Some(())
      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Cause`.
   */
  implicit val IdentityFlattenCause: IdentityFlatten[Cause] =
    new IdentityFlatten[Cause] {
      override def any: Cause[Any] = Cause.empty

      override def flatten[A](ffa: Cause[Cause[A]]): Cause[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Chunk`.
   */
  implicit val IdentityFlattenChunk: IdentityFlatten[Chunk] =
    new IdentityFlatten[Chunk] {
      def any: Chunk[Any] = Chunk.unit

      def flatten[A](ffa: Chunk[Chunk[A]]): Chunk[A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `Exit`.
   */
  implicit def IdentityFlattenExit[E]: IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def any: Exit[E, Any] = Exit.unit

      def flatten[A](ffa: Exit[E, Exit[E, A]]): Exit[E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZIO`.
   */
  implicit def IdentityFlattenZIO[R, E]: IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def any: ZIO[R, E, Any] = ZIO.unit

      def flatten[A](ffa: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZManaged`.
   */
  implicit def IdentityFlattenZManaged[R, E]: IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def any: ZManaged[R, E, Any] = ZManaged.unit

      def flatten[A](ffa: ZManaged[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] = ffa.flatten
    }

  /**
   * The `IdentityFlatten` instance for `ZStream`.
   */
  implicit def IdentityFlattenZStream[R, E]: IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def any: ZStream[R, E, Any] = ZStream.unit

      def flatten[A](ffa: ZStream[R, E, ZStream[R, E, A]]): ZStream[R, E, A] = ffa.flatten
    }
}
