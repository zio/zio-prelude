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
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[Either[A, B]]`.
   */
  def identity: F[Nothing]

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object IdentityEitherF extends LawfulF.Invariant[EqualFIdentityEitherFInvariant, Equal] {

  /**
   * For all `fa`, `either(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityEitherFInvariant, Equal]("leftIdentityLaw") {
    def apply[F[_]: EqualFIdentityEitherFInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityEitherF[F].either[Nothing, A](IdentityEitherF[F].identity, fa)
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
      val left  = IdentityEitherF[F].either[A, Nothing](fa, IdentityEitherF[F].identity)
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
      val identity: Option[Nothing] =
        None
    }
}
