package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.EqualFIdentityEitherFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[Either[A, B]]` with an identity value.
 */
@implicitNotFound("No implicit IdentityEitherF defined for ${F}.")
trait IdentityEitherF[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[Either[A, B]]`.
   */
  def none: F[Nothing]
}

object IdentityEitherF extends LawfulF.Invariant[EqualFIdentityEitherFInvariant, Equal] {

  /**
   * For all `fa`, `either(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityEitherFInvariant, Equal]("leftIdentityLaw") {
    def apply[F[_]: EqualFIdentityEitherFInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityEitherF[F].either[Nothing, A](IdentityEitherF[F].none, fa)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A] compose Equivalence.eitherFlip).to(left)
      left2 <-> right
    }
  }

  /**
   * For all `fa`, `either(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityEitherFInvariant, Equal]("rightOdentityLaw") {
    def apply[F[_]: EqualFIdentityEitherFInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityEitherF[F].either[A, Nothing](fa, IdentityEitherF[F].none)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `IdentityEitherF` must satisfy.
   */
  val laws = leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `IdentityEitherF[F]`.
   */
  def apply[F[_]](implicit identityEitherF: IdentityEitherF[F]): IdentityEitherF[F] =
    identityEitherF

  /**
   * The `IdentityEitherF` instance for `Option`.
   */
  implicit val OptionIdentityEitherF: IdentityEitherF[Option] =
    new IdentityEitherF[Option] {
      def either[A, B](fa: => Option[A], fb: => Option[B]): Option[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
      val none: Option[Nothing] =
        None
    }
}

trait IdentityEitherFSyntax {

  /**
   * Provides infix syntax for identity operations for invariant types.
   */
  implicit class IdentityEitherFOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEitherIdentity[B](fb: => F[B])(implicit either: IdentityEitherF[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for identity operations for covariant types.
   */
  implicit class IdentityEitherFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElseIdentity(fa2: => F[A])(implicit either: IdentityEitherF[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for identity operations for contravariant types.
   */
  implicit class IdentityEitherFContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWithIdentity[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: IdentityEitherF[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
