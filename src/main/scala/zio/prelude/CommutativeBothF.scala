package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.CommutativeBothFEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit CommutativeBothF defined for ${F}.")
trait CommutativeBothF[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

object CommutativeBothF extends LawfulF.Invariant[CommutativeBothFEqualFInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `both(fa, fb)` is equivalent to `both(fb, fa)`.
   */
  val commutativeLaw = new LawsF.Invariant.Law2[CommutativeBothFEqualFInvariant, Equal]("commutativeLaw") {
    def apply[F[_]: CommutativeBothFEqualFInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
      val left  = fa.zipPar(fb)
      val right = fb.zipPar(fa)
      val left2 = Invariant[F].invmap(Equivalence.tupleFlip[A, B]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `CommutativeBothF` must satisfy.
   */
  val laws = commutativeLaw

  /**
   * Summons an implicit `CommutativeBothF[F]`.
   */
  def apply[F[+_]](implicit commutativeBothF: CommutativeBothF[F]): CommutativeBothF[F] =
    commutativeBothF

  /**
   * The `CommutativeBothF` instance for `Option`.
   */
  implicit val OptionCommutativeBothF: CommutativeBothF[Option] =
    new CommutativeBothF[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }
}

trait CommutativeBothFSyntax {

  /**
   * Provides infix syntax for commutative operations for invariant types.
   */
  implicit class CommutativeBothFOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zipPar[B](fb: => F[B])(implicit both: CommutativeBothF[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for commutative operations for covariant types.
   */
  implicit class CommutativeBothFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWithPar[B, C](
      fb: => F[B]
    )(f: (A, B) => C)(implicit both: CommutativeBothF[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for commutative operations for contravariant types.
   */
  implicit class CommutativeBothFContraVariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWithPar[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: CommutativeBothF[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
