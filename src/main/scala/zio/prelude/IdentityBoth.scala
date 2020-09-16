package zio.prelude

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.{ Success, Try }

import zio.prelude.coherent.DeriveEqualIdentityBothInvariant
import zio.prelude.newtypes.Failure
import zio.test.TestResult
import zio.test.laws._

import com.github.ghik.silencer.silent

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[(A, B)]` with an identity.
 */
@implicitNotFound("No implicit IdentityBoth defined for ${F}.")
trait IdentityBoth[F[_]] extends AssociativeBoth[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[(A, B)]`.
   */
  def any: F[Any]
}

@silent("Unused import")
object IdentityBoth extends LawfulF.Invariant[DeriveEqualIdentityBothInvariant, Equal] {
  import zio._ // for zio.EitherCompat

  /**
   * For all `fa`, `both(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("leftIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(IdentityBoth[F].any, fa)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A] compose Equivalence.tupleFlip).to(left)
        left2 <-> right
      }
    }

  /**
   * For all `fa`, `both(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("rightIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(fa, IdentityBoth[F].any)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `IdentityBoth` must satisfy.
   */
  val laws: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `IdentityBoth[F]`.
   */
  def apply[F[_]](implicit identityBoth: IdentityBoth[F]): IdentityBoth[F] =
    identityBoth
}

trait IdentityBothSyntax {
  implicit class IdentityBothAnyOps[A](a: => A) {
    def succeed[F[+_]](implicit both: IdentityBoth[F], covariant: Covariant[F]): F[A] =
      both.any.map(_ => a)
  }

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
