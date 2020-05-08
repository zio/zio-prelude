package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.EqualFIdentityBothInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[(A, B)]` with an identity.
 */
@implicitNotFound("No implicit IdentityBoth defined for ${F}.")
trait IdentityBoth[F[_]] {

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

object IdentityBoth extends LawfulF.Invariant[EqualFIdentityBothInvariant, Equal] {

  /**
   * For all `fa`, `both(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityBothInvariant, Equal]("leftIdentityLaw") {
    def apply[F[_]: EqualFIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityBoth[F].both(IdentityBoth[F].identity, fa)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.tupleAny[A] compose Equivalence.tupleFlip).to(left)
      left2 <-> right
    }
  }

  /**
   * For all `fa`, `both(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw = new LawsF.Invariant.Law1[EqualFIdentityBothInvariant, Equal]("rightIdentityLaw") {
    def apply[F[_]: EqualFIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
      val left  = IdentityBoth[F].both(fa, IdentityBoth[F].identity)
      val right = fa
      val left2 = Invariant[F].invmap(Equivalence.tupleAny[A]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `IdentityBoth` must satisfy.
   */
  val laws = leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `IdentityBoth[F]`.
   */
  def apply[F[_]](implicit identityBoth: IdentityBoth[F]): IdentityBoth[F] =
    identityBoth

  /**
   * The `IdentityBoth` instance for `Option`.
   */
  implicit val OptionidentityBoth: IdentityBoth[Option] =
    new IdentityBoth[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
      val identity: Option[Any] =
        Some(())
    }
}

trait IdentityBothSyntax {

  /**
   * Provides infix syntax for identity operations for invariant types.
   */
  implicit class IdentityBothOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zipIdentity[B](fb: => F[B])(implicit both: IdentityBoth[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for identity operations for covariant types.
   */
  implicit class IdentityBothCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWithIdentity[B, C](
      fb: => F[B]
    )(f: (A, B) => C)(implicit both: IdentityBoth[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for identity operations for contravariant types.
   */
  implicit class IdentityBothContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWithIdentity[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: IdentityBoth[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
