package zio.prelude

import scala.concurrent.{ ExecutionContext, Future, Promise }

import zio.ZIO
import zio.prelude.coherent.CommutativeEitherDeriveEqualInvariant
import zio.stream.{ ZSink, ZStream }
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit CommutativeEither defined for ${F}.")
trait CommutativeEither[F[_]] extends AssociativeEither[F] {
  // def eitherPar[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object CommutativeEither extends LawfulF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `either(fa, fb)` is equivalent to `either(fb, fa)`.
   */
  val commutativeLaw: LawsF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law2[CommutativeEitherDeriveEqualInvariant, Equal]("commutativeLaw") {
      def apply[F[_]: CommutativeEitherDeriveEqualInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.orElseEitherPar(fb)
        val right = fb.orElseEitherPar(fa)
        val left2 = Invariant[F].invmap(Equivalence.eitherFlip[A, B]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `CommutativeEither` must satisfy.
   */
  val laws: LawsF.Invariant[CommutativeEitherDeriveEqualInvariant, Equal] =
    commutativeLaw

  /**
   * The `CommutativeEither` instance for `Future`.
   */
  implicit def FutureCommutativeEither(implicit ec: ExecutionContext): CommutativeEither[Future] =
    new CommutativeEither[Future] {
      def either[A, B](fa: => Future[A], fb: => Future[B]): Future[Either[A, B]] =
        Promise[Either[A, B]].completeWith(fa.map(Left(_))).completeWith(fb.map(Right(_))).future
    }

  /**
   * The `CommutativeEither` instance for `ZIO`.
   */
  implicit def ZIOCommutativeEither[R, E]: CommutativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def either[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, Either[A, B]] =
        fa.raceEither(fb)
    }

  /**
   * The `CommutativeEither` instance for `ZSink`.
   */
  implicit def ZSinkCommutativeEither[R, E, I, L]
    : CommutativeEither[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZSink[R, E, I, L, a] })#lambda] {
      def either[A, B](fa: => ZSink[R, E, I, L, A], fb: => ZSink[R, E, I, L, B]): ZSink[R, E, I, L, Either[A, B]] =
        fa.raceBoth(fb)
    }

  /**
   * The `CommutativeEither` instance for `ZStream`.
   */
  implicit def ZStreamCommutativeEither[R, E]: CommutativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new CommutativeEither[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def either[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, Either[A, B]] =
        fa mergeEither fb
    }

  /**
   * Summons an implicit `CommutativeEither[F]`.
   */
  def apply[F[_]](implicit commutativeEither: CommutativeEither[F]): CommutativeEither[F] =
    commutativeEither
}

trait CommutativeEitherSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeEitherOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEitherPar`.
     */
    def <|>[B](fb: => F[B])(implicit either: CommutativeEither[F]): F[Either[A, B]] =
      orElseEitherPar(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEitherPar[B](fb: => F[B])(implicit either: CommutativeEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElsePar(fa2: => F[A])(implicit either: CommutativeEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWithPar[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: CommutativeEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
