package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.AssociativeBothEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit AssociativeBoth defined for ${F}.")
trait AssociativeBoth[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

object AssociativeBoth extends LawfulF.Invariant[AssociativeBothEqualFInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `both(fa, both(fb, fc))` is equivalent
   * to `both(both(fa, fb), fc)`.
   */
  val associativityLaw = new LawsF.Invariant.Law3[AssociativeBothEqualFInvariant, Equal]("associativityLaw") {
    def apply[F[_]: AssociativeBothEqualFInvariant, A: Equal, B: Equal, C: Equal](
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
   * The set of law laws that instances of `AssociativeBoth` must satisfy.
   */
  val laws = associativityLaw

  /**
   * Summons an implicit `AssociativeBoth[F]`.
   */
  def apply[F[+_]](implicit associativeBoth: AssociativeBoth[F]): AssociativeBoth[F] =
    associativeBoth

  /**
   * The `AssociativeBoth` instance for `Option`.
   */
  implicit val OptionAssociativeBoth: AssociativeBoth[Option] =
    new AssociativeBoth[Option] {
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }

  /**
   * The `AssociativeBoth` instance for `Id`.
   */
  implicit val IdAssociativeBoth: AssociativeBoth[Id] =
    new AssociativeBoth[Id] {
      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] = Id((Id.unwrap(fa), Id.unwrap(fb)))
    }
}

trait AssociativeBothSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeBothOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zip`.
     */
    def <*>[B](fb: => F[B])(implicit both: AssociativeBoth[F]): F[(A, B)] =
      zip(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zip[B](fb: => F[B])(implicit both: AssociativeBoth[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeBothCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWith[B, C](fb: => F[B])(f: (A, B) => C)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeBothContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWith[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: AssociativeBoth[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }
}
