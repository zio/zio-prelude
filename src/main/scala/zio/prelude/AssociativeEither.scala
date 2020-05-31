package zio.prelude

import scala.annotation.implicitNotFound
import scala.util.Try

import zio.prelude.coherent.AssociativeEitherEqualFInvariant
import zio.test.TestResult
import zio.test.laws._

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[Either[A, B]]`.
 */
@implicitNotFound("No implicit AssociativeEither defined for ${F}.")
trait AssociativeEither[F[_]] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an
   * `F[Either[A, B]]`.
   */
  def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object AssociativeEither extends LawfulF.Invariant[AssociativeEitherEqualFInvariant, Equal] {

  /**
   * For all `fa`, `fb`, and `fc`, `either(fa, either(fb, fc))` is
   * equivalent to `either(either(fa, fb), fc)`.
   */
  val associativityLaw = new LawsF.Invariant.Law3[AssociativeEitherEqualFInvariant, Equal]("associativityLaw") {
    def apply[F[_]: AssociativeEitherEqualFInvariant, A: Equal, B: Equal, C: Equal](
      fa: F[A],
      fb: F[B],
      fc: F[C]
    ): TestResult = {
      val left  = fa.orElseEither(fb.orElseEither(fc))
      val right = (fa.orElseEither(fb)).orElseEither(fc)
      val left2 = Invariant[F].invmap(Equivalence.either[A, B, C]).to(left)
      left2 <-> right
    }
  }

  /**
   * The set of law laws that instances of `AssociativeEither` must
   * satisfy.
   */
  val laws: ZLawsF.Invariant[AssociativeEitherEqualFInvariant, Equal, Any] =
    associativityLaw

  /**
   * Summons an implicit `AssociativeEither[F]`.
   */
  def apply[F[+_]](implicit associativeEither: AssociativeEither[F]): AssociativeEither[F] =
    associativeEither

  /**
   * The `AssociativeEither` instance for `Option`.
   */
  implicit val OptionAssociativeEither: AssociativeEither[Option] =
    new AssociativeEither[Option] {
      def either[A, B](fa: => Option[A], fb: => Option[B]): Option[Either[A, B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }

  /**
   * The `AssociativeEither` instance for `Try`.
   */
  implicit val TryAssociativeEither: AssociativeEither[Try] =
    new AssociativeEither[Try] {
      def either[A, B](fa: => Try[A], fb: => Try[B]): Try[Either[A,B]] =
        fa.map(Left(_)) orElse fb.map(Right(_))
    }
}

trait AssociativeEitherSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeEitherOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `orElseEither`.
     */
    def <+>[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      orElseEither(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEither[B](fb: => F[B])(implicit either: AssociativeEither[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeEitherCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElse(fa2: => F[A])(implicit either: AssociativeEither[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeEitherContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWith[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: AssociativeEither[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
