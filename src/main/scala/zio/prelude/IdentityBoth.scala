package zio.prelude

import scala.annotation.implicitNotFound
import zio.prelude.coherent.DeriveEqualIdentityBothInvariant
import zio.prelude.newtypes.Failure
import zio.test.TestResult
import zio.test.laws._

import scala.concurrent.Future
import scala.util.{ Success, Try }

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

object IdentityBoth extends LawfulF.Invariant[DeriveEqualIdentityBothInvariant, Equal] {

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

  /**
   * The `IdentityBoth` instance for `Either`.
   */
  implicit def EitherIdentityBoth[L]: IdentityBoth[({ type lambda[+r] = Either[L, r] })#lambda] =
    new IdentityBoth[({ type lambda[+r] = Either[L, r] })#lambda] {
      val any: Either[L, Any] = Right(())

      def both[A, B](fa: => Either[L, A], fb: => Either[L, B]): Either[L, (A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityBoth` instance for a failed `Either`
   */
  implicit def EitherFailedIdentityBoth[R]: IdentityBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new IdentityBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      val any: Failure[Either[Any, R]] = Failure.wrap(Left(()))

      def both[A, B](fa: => Failure[Either[A, R]], fb: => Failure[Either[B, R]]): Failure[Either[(A, B), R]] =
        Failure.wrap {
          Failure
            .unwrap(fa)
            .left
            .flatMap(a => Failure.unwrap(fb).left.map(b => (a, b)))
        }
    }

  /**
   * The `IdentityBoth` instance for `Future`.
   */
  implicit val FutureIdentityBoth: IdentityBoth[Future] =
    new IdentityBoth[Future] {
      val any: Future[Any] = Future.unit

      def both[A, B](fa: => Future[A], fb: => Future[B]): Future[(A, B)] = fa zip fb
    }

  /**
   * The `IdentityBoth` instance for `Option`.
   */
  implicit val OptionIdentityBoth: IdentityBoth[Option] =
    new IdentityBoth[Option] {
      val any: Option[Any] =
        Some(())
      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }

  /**
   * The `IdentityBoth` instance for `List`.
   */
  implicit val ListAssociativeIdentityBoth: IdentityBoth[List] =
    new IdentityBoth[List] {
      val any: List[Any] =
        List(())
      def both[A, B](fa: => List[A], fb: => List[B]): List[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityBoth` instance for `Id`.
   */
  implicit val IdIdentityBoth: IdentityBoth[Id] =
    new IdentityBoth[Id] {
      val any: Id[Any] = Id(())

      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] =
        Id(Id.unwrap(fa) -> Id.unwrap(fb))
    }

  /**
   * The `IdentityBoth` instance for `Try`.
   */
  implicit val TryIdentityBoth: IdentityBoth[Try] =
    new IdentityBoth[Try] {
      val any: Try[Any] = Success(())

      def both[A, B](fa: => Try[A], fb: => Try[B]): Try[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }
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
