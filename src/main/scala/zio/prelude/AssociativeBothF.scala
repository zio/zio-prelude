package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.AssociativeBothFEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit AssociativeBothF defined for ${F}.")
trait AssociativeBothF[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

object AssociativeBothF extends LawfulF.Invariant[AssociativeBothFEqualFInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `both(fa, both(fb, fc))` is equivalent
   * to `both(both(fa, fb), fc)`.
   */
  val associativityLaw = new LawsF.Invariant.Law3[AssociativeBothFEqualFInvariant, Equal]("associativityLaw") {
    def apply[F[_]: AssociativeBothFEqualFInvariant, A: Equal, B: Equal, C: Equal](
      fa: F[A],
      fb: F[B],
      fc: F[C]
    ): TestResult = {
      val left  = fa.zip(fb.zip(fc))
      val right = (fa.zip(fb)).zip(fc)
      val left2 = Invariant[F].invmap(Equivalence.tuple[A, B, C]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `AssociativeBothF` must satisfy.
   */
  val laws = associativityLaw

  /**
   * Summons an implicit `AssociativeBothF[F]`.
   */
  def apply[F[+_]](implicit associativeBothF: AssociativeBothF[F]): AssociativeBothF[F] =
    associativeBothF

  /**
   * The `AssociativeBothF` instance for `Option`.
   */
  implicit val OptionAssociativeBothF: AssociativeBothF[Option] =
    new AssociativeBothF[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }
}

trait AssociativeBothFSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeBothFOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zip`.
     */
    def <*>[B](fb: => F[B])(implicit both: AssociativeBothF[F]): F[(A, B)] =
      zip(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zip[B](fb: => F[B])(implicit both: AssociativeBothF[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeBothFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWith[B, C](fb: => F[B])(f: (A, B) => C)(implicit both: AssociativeBothF[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeBothFContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWith[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: AssociativeBothF[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
