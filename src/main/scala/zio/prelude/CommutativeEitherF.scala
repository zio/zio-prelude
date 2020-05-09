package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.CommutativeEitherFEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit CommutativeEitherF defined for ${F}.")
trait CommutativeEitherF[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object CommutativeEitherF extends LawfulF.Invariant[CommutativeEitherFEqualFInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `either(fa, fb)` is equivalent to `either(fb, fa)`.
   */
  val commutativeLaw = new LawsF.Invariant.Law2[CommutativeEitherFEqualFInvariant, Equal]("commutativeLaw") {
    def apply[F[_]: CommutativeEitherFEqualFInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
      val left  = fa.orElseEitherPar(fb)
      val right = fb.orElseEitherPar(fa)
      val left2 = Invariant[F].invmap(Equivalence.eitherFlip[A, B]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `CommutativeEitherF` must satisfy.
   */
  val laws = commutativeLaw

  /**
   * Summons an implicit `CommutativeEitherF[F]`.
   */
  def apply[F[+_]](implicit commutativeEitherF: CommutativeEitherF[F]): CommutativeEitherF[F] =
    commutativeEitherF
}

trait CommutativeEitherFSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeEitherFOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEitherPar`.
     */
    def <|>[B](fb: => F[B])(implicit either: CommutativeEitherF[F]): F[Either[A, B]] =
      orElseEitherPar(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEitherPar[B](fb: => F[B])(implicit either: CommutativeEitherF[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeEitherFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElsePar(fa2: => F[A])(implicit either: CommutativeEitherF[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeEitherFContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWithPar[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: CommutativeEitherF[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
