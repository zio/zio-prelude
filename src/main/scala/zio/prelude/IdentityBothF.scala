package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.EqualFIdentityBothFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[(A, B)]` with an identity.
 */
@implicitNotFound("No implicit IdentityBothF defined for ${F}.")
trait IdentityBothF[F[_]] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[(A, B)]`.
   */
  def identity: F[Any]

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

object IdentityBothF extends LawfulF.Invariant[EqualFIdentityBothFInvariant, Equal] {

  /**
   * For all `fa`, `both(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityBothFInvariant, Equal]("leftIdentityLaw") {
    def apply[F[_]: EqualFIdentityBothFInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityBothF[F].both(IdentityBothF[F].identity, fa)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.tupleAny[A] compose Equivalence.tupleFlip).to(left)
      left2 <-> right
    }
  }

  /**
   * For all `fa`, `both(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityBothFInvariant, Equal]("rightIdentityLaw") {
    def apply[F[_]: EqualFIdentityBothFInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityBothF[F].both(fa, IdentityBothF[F].identity)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.tupleAny[A]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `IdentityBothF` must satisfy.
   */
  val laws = leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `IdentityBothF[F]`.
   */
  def apply[F[_]](implicit identityBothF: IdentityBothF[F]): IdentityBothF[F] =
    identityBothF

  /**
   * The `IdentityBothF` instance for `Option`.
   */
  implicit val OptionidentityBothF: IdentityBothF[Option] =
    new IdentityBothF[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
      val identity: Option[Any] =
        Some(())
    }
}
