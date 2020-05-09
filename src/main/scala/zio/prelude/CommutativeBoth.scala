package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.CommutativeBothEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit CommutativeBoth defined for ${F}.")
trait CommutativeBoth[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

object CommutativeBoth extends LawfulF.Invariant[CommutativeBothEqualFInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `both(fa, fb)` is equivalent to `both(fb, fa)`.
   */
  val commutativeLaw = new LawsF.Invariant.Law2[CommutativeBothEqualFInvariant, Equal]("commutativeLaw") {
    def apply[F[_]: CommutativeBothEqualFInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
      val left  = fa.zipPar(fb)
      val right = fb.zipPar(fa)
      val left2 = Invariant[F].invmap(Equivalence.tupleFlip[A, B]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `CommutativeBoth` must satisfy.
   */
  val laws = commutativeLaw

  /**
   * Summons an implicit `CommutativeBoth[F]`.
   */
  def apply[F[+_]](implicit commutativeBoth: CommutativeBoth[F]): CommutativeBoth[F] =
    commutativeBoth

  /**
   * The `CommutativeBoth` instance for `Option`.
   */
  implicit val OptionCommutativeBoth: CommutativeBoth[Option] =
    new CommutativeBoth[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }
}

trait CommutativeBothSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeBothOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zipPar`.
     */
    def <&>[B](fb: => F[B])(implicit both: CommutativeBoth[F]): F[(A, B)] =
      zipPar(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zipPar[B](fb: => F[B])(implicit both: CommutativeBoth[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeBothCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWithPar[B, C](
      fb: => F[B]
    )(f: (A, B) => C)(implicit both: CommutativeBoth[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeBothContraVariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWithPar[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: CommutativeBoth[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
