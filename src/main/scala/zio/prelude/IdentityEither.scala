package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.DeriveEqualIdentityEitherInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[Either[A, B]]` with an identity value.
 */
@implicitNotFound("No implicit IdentityEither defined for ${F}.")
trait IdentityEither[F[_]] extends AssociativeEither[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[Either[A, B]]`.
   */
  def none: F[Nothing]
}

object IdentityEither extends LawfulF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] {

  /**
   * For all `fa`, `either(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityEitherInvariant, Equal]("leftIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityEitherInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityEither[F].either[Nothing, A](IdentityEither[F].none, fa)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A] compose Equivalence.eitherFlip).to(left)
        left2 <-> right
      }
    }

  /**
   * For all `fa`, `either(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityEitherInvariant, Equal]("rightIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityEitherInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityEither[F].either[A, Nothing](fa, IdentityEither[F].none)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `IdentityEither` must satisfy.
   */
  val laws: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `IdentityEither[F]`.
   */
  def apply[F[_]](implicit identityEither: IdentityEither[F]): IdentityEither[F] =
    identityEither
}

trait IdentityEitherSyntax {

  /**
   * Provides infix syntax for identity operations for invariant types.
   */
  implicit class IdentityEitherOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEitherIdentity[B](fb: => F[B])(implicit either: IdentityEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for identity operations for covariant types.
   */
  implicit class IdentityEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElseIdentity(fa2: => F[A])(implicit either: IdentityEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for identity operations for contravariant types.
   */
  implicit class IdentityEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWithIdentity[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: IdentityEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
