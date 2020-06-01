package zio.prelude

import zio._
import zio.prelude.coherent.AssociativeEitherEqualFInvariant
import zio.prelude.newtypes.Failure
import zio.stream.ZStream
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit AssociativeEither defined for ${F}.")
trait AssociativeEither[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object AssociativeEither extends LawfulF.Invariant[AssociativeEitherEqualFInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `either(fa, either(fb, fc))` is
   * equivalent to `either(either(fa, fb), fc)`.
   */
  val associativityLaw = new LawsF.Invariant.Law3[AssociativeEitherEqualFInvariant, Equal]("associativityLaw") {
    def apply[F[_]: AssociativeEitherEqualFInvariant, A: Equal, B: Equal, C: Equal](
      fa: F[A],
      fb: F[B],
      fc: F[C]
    ): TestResult = {
      val left  = fa.orElseEither(fb.orElseEither(fc))
      val right = (fa.orElseEither(fb)).orElseEither(fc)
      val left2 = Invariant[F].invmap(Equivalence.either[A, B, C]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `AssociativeEither` must
   * satisfy.
   */
  val laws: ZLawsF.Invariant[AssociativeEitherEqualFInvariant, Equal, Any] =
    associativityLaw

  /**
   * Summons an implicit `AssociativeEither[F]`.
   */
  def apply[F[+_]](implicit associativeEither: AssociativeEither[F]): AssociativeEither[F] =
    associativeEither

  /**
   * The `AssociativeEither` instance for `Either`.
   */
  implicit def EitherAssociativeEither[L]: AssociativeEither[({ type lambda[+r] = Either[L, r] })#lambda] =
    new AssociativeEither[({ type lambda[+r] = Either[L, r] })#lambda] {
      def either[A, B](fa: => Either[L, A], fb: => Either[L, B]): Either[L, Either[A, B]] =
        fa.map(Left(_)).left.flatMap(_ => fb.map(Right(_)))
    }

  /**
   * The `AssociativeEither` instance for a failed `Either`
   */
  implicit def EitherFailedAssociativeEither[R]
    : AssociativeEither[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new AssociativeEither[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      def either[A, B](fa: => Failure[Either[A, R]], fb: => Failure[Either[B, R]]): Failure[Either[Either[A, B], R]] =
        Failure.wrap {
          Failure
            .unwrap(fa)
            .left
            .map(Left(_))
            .flatMap(_ => Failure.unwrap(fb).left.map(Right(_)))
        }
    }

  /**
   * The `AssociativeEither` instance for `Future`.
   */
  implicit def FutureAssociativeEither(implicit ec: ExecutionContext): AssociativeEither[Future] =
    new AssociativeEither[Future] {
      def either[A, B](fa: => Future[A], fb: => Future[B]): Future[Either[A, B]] =
        fa.map(Left(_)).recoverWith {
          case _: Throwable => fb.map(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `Option`.
   */
  implicit val OptionAssociativeEither: AssociativeEither[Option] =
    new AssociativeEither[Option] {
      def either[A, B](fa: => Option[A], fb: => Option[B]): Option[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for `Try`.
   */
  implicit val TryAssociativeEither: AssociativeEither[Try] =
    new AssociativeEither[Try] {
      def either[A, B](fa: => Try[A], fb: => Try[B]): Try[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for `ZIO`.
   */
  implicit def ZIOAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def either[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for failed `ZIO`.
   */
  implicit def ZIOFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `ZLayer`.
   */
  implicit def ZLayerAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def either[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for `ZManaged`.
   */
  implicit def ZManagedAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def either[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }

  /**
   * The `AssociativeEither` instance for `ZStream`.
   */
  implicit def ZStreamAssociativeEither[R, E]: AssociativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new AssociativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def either[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for failed `ZStream`.
   */
  implicit def ZStreamFailureAssociativeEither[R, A]
    : AssociativeEither[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] =
    new AssociativeEither[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] {
      def either[EA, EB](
        fa: => Failure[ZStream[R, EA, A]],
        fb: => Failure[ZStream[R, EB, A]]
      ): Failure[ZStream[R, Either[EA, EB], A]] =
        Failure.wrap {
          Failure.unwrap(fa).mapError(Left(_)) *> Failure.unwrap(fb).mapError(Right(_))
        }
    }
}

trait AssociativeEitherSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeEitherOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEither`.
     */
    def <+>[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      orElseEither(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEither[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElse(fa2: => F[A])(implicit either: AssociativeEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWith[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: AssociativeEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
