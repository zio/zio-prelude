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
  def any: F[Any]

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
      val left  = IdentityBothF[F].both(IdentityBothF[F].any, fa)
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
      val left  = IdentityBothF[F].both(fa, IdentityBothF[F].any)
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
      val any: Option[Any] =
        Some(())
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }
}

trait IdentityBothFSyntax {

  /**
   * Provides infix syntax for identity operations for invariant types.
   */
  implicit class IdentityBothFOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zipIdentity[B](fb: => F[B])(implicit both: IdentityBothF[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for identity operations for covariant types.
   */
  implicit class IdentityBothFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWithIdentity[B, C](
      fb: => F[B]
    )(f: (A, B) => C)(implicit both: IdentityBothF[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for identity operations for contravariant types.
   */
  implicit class IdentityBothFContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWithIdentity[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: IdentityBothF[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
