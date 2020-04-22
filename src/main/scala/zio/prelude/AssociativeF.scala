package zio.prelude

import zio.prelude.coherent.{ BothEqualFInvariant, EitherEqualFInvariant }
import zio.test.TestResult
import zio.test.laws._

/**
 * The `AssociativeF` type class describes as associative binary operator for
 * a parameterized type `F[_]`. Because the binary operator must be able to
 * combine values of types `F[A]` and `F[B]` for any `A` and `B`, the operator
 * cannot actually combine the underlying values but must either return both
 * of them or either of them.
 */
object AssociativeF {
  // AssociativeF.Both    => AssociativeBothF
  // AssociativeF.Either  => AssociativeEitherF

  /**
   * An associative binary operator that combines two values of types `F[A]`
   * and `F[B]` to produce an `F[(A, B)]`.
   */
  trait Both[F[_]] {
    def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
  }

  object Both extends LawfulF.Invariant[BothEqualFInvariant, Equal] {

    /**
     * For all `fa`, `fb`, and `fc`, `both(fa, both(fb, fc))` is equivalent
     * to `both(both(fa, fb), fc)`.
     */
    val associativityLaw = new LawsF.Invariant.Law3[BothEqualFInvariant, Equal]("associativityLaw") {
      def apply[F[_]: BothEqualFInvariant, A: Equal, B: Equal, C: Equal](fa: F[A], fb: F[B], fc: F[C]): TestResult = {
        val left  = fa.zip(fb.zip(fc))
        val right = (fa.zip(fb)).zip(fc)
        val left2 = Invariant[F].invmap(Equivalence.tuple[A, B, C]).to(left)
        left2 <-> right
      }
    }

    /**
     * The set of law laws that instances of `AssociativeF.Both` must satisfy.
     */
    val laws = associativityLaw

    /**
     * The `Both` instance for `Option`.
     */
    implicit val OptionBoth: Both[Option] =
      new Both[Option] {
        def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some((a, b))
            case _                  => None
          }
      }
  }

  /**
   * An associative binary operator that combines two values of types `F[A]`
   * and `F[B]` to produce an `F[Either[A, B]]`.
   */
  trait Either[F[_]] {
    def either[A, B](fa: => F[A], fb: => F[B]): F[scala.util.Either[A, B]]
  }

  object Either extends LawfulF.Invariant[EitherEqualFInvariant, Equal] {

    /**
     * For all `fa`, `fb`, and `fc`, `either(fa, either(fb, fc))` is
     * equivalent to `either(either(fa, fb), fc)`.
     */
    val associativityLaw = new LawsF.Invariant.Law3[EitherEqualFInvariant, Equal]("associativityLaw") {
      def apply[F[_]: EitherEqualFInvariant, A: Equal, B: Equal, C: Equal](fa: F[A], fb: F[B], fc: F[C]): TestResult = {
        val left  = fa.orElseEither(fb.orElseEither(fc))
        val right = (fa.orElseEither(fb)).orElseEither(fc)
        val left2 = Invariant[F].invmap(Equivalence.either[A, B, C]).to(left)
        left2 <-> right
      }
    }

    /**
     * The set of law laws that instances of `AssociativeF.Either` must
     * satisfy.
     */
    val laws: ZLawsF.Invariant[EitherEqualFInvariant, Equal, Any] =
      associativityLaw

    /**
     * The `Either` instance for `Option`.
     */
    implicit val OptionEither: Either[Option] =
      new Either[Option] {
        def either[A, B](fa: => Option[A], fb: => Option[B]): Option[scala.util.Either[A, B]] =
          fa.map(Left(_)) orElse fb.map(Right(_))
      }
  }
}

trait AssociativeFSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeFOps[F[_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zip[B](fb: => F[B])(implicit both: AssociativeF.Both[F]): F[(A, B)] =
      both.both(fa, fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]`.
     */
    def orElseEither[B](fb: => F[B])(implicit either: AssociativeF.Either[F]): F[Either[A, B]] =
      either.either(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeFCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWith[B, C](fb: => F[B])(f: (A, B) => C)(implicit both: AssociativeF.Both[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)

    /**
     * Combines two values of types `F[A]` and `F[A]` to produce an
     * `F[Either[A, A]]` and then merges the result.
     */
    def orElse(fa2: => F[A])(implicit either: AssociativeF.Either[F], covariant: Covariant[F]): F[A] =
      either.either(fa, fa2).map(_.merge)
  }

  implicit class AssociativeFContraVariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWith[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: AssociativeF.Both[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[Either[A, B]]` and then contramaps the result with the specified
     * function.
     */
    def eitherWith[B, C](
      fb: => F[B]
    )(f: C => Either[A, B])(implicit either: AssociativeF.Either[F], contravariant: Contravariant[F]): F[C] =
      either.either(fa, fb).contramap(f)
  }
}
