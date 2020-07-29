package zio.prelude

import zio._
import zio.prelude.coherent.CommutativeBothDeriveEqualInvariant
import zio.prelude.newtypes.{ AndF, Failure, OrF }
import zio.stream.{ ZSink, ZStream }
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound

/**
 * A commutative binary operator that combines two values of types `F[A]` and
 * `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit CommutativeBoth defined for ${F}.")
trait CommutativeBoth[F[_]] extends AssociativeBoth[F] {

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]

}

object CommutativeBoth extends LawfulF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] {

  /**
   * For all `fa` and `fb`, `both(fa, fb)` is equivalent to `both(fb, fa)`.
   */
  val commutativeLaw: LawsF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law2[CommutativeBothDeriveEqualInvariant, Equal]("commutativeLaw") {
      def apply[F[_]: CommutativeBothDeriveEqualInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.zipPar(fb)
        val right = fb.zipPar(fa)
        val left2 = Invariant[F].invmap(Equivalence.tupleFlip[A, B]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `CommutativeBoth` must satisfy.
   */
  val laws: LawsF.Invariant[CommutativeBothDeriveEqualInvariant, Equal] =
    commutativeLaw

  /**
   * Summons an implicit `CommutativeBoth[F]`.
   */
  def apply[F[+_]](implicit commutativeBoth: CommutativeBoth[F]): CommutativeBoth[F] =
    commutativeBoth

  /**
   * The `CommutativeBoth` instance for `Chunk`.
   */
  implicit def ChunkCommutativeBoth: CommutativeBoth[Chunk] =
    new CommutativeBoth[Chunk] {
      def both[A, B](fa: => Chunk[A], fb: => Chunk[B]): Chunk[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `Exit`.
   */
  implicit def ExitCommutativeBoth[E]: CommutativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def both[A, B](fa: => Exit[E, A], fb: => Exit[E, B]): Exit[E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `Id`.
   */
  implicit val IdCommutativeBoth: CommutativeBoth[Id] =
    new CommutativeBoth[Id] {
      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] = Id((Id.unwrap(fa), Id.unwrap(fb)))
    }

  /**
   * The `CommutativeBoth` instance for `List`.
   */
  implicit def ListCommutativeBoth: CommutativeBoth[List] =
    new CommutativeBoth[List] {
      def both[A, B](fa: => List[A], fb: => List[B]): List[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `NonEmptyChunk`.
   */
  implicit def NonEmptyChunkCommutativeBoth: CommutativeBoth[NonEmptyChunk] =
    new CommutativeBoth[NonEmptyChunk] {
      def both[A, B](fa: => NonEmptyChunk[A], fb: => NonEmptyChunk[B]): NonEmptyChunk[(A, B)] =
        (fa zipWith fb)((_, _))
    }

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

  /**
   * The `CommutativeBoth` instance for And `Schedule`.
   */
  implicit def ScheduleAndCommutativeBoth[R, E]
    : CommutativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => AndF[Schedule[R, E, A]], fb: => AndF[Schedule[R, E, B]]): AndF[Schedule[R, E, (A, B)]] =
        AndF.wrap {
          AndF.unwrap(fa) && AndF.unwrap(fb)
        }
    }

  /**
   * The `AssociativeBoth` instance for Or `Schedule`.
   */
  implicit def ScheduleOrCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => OrF[Schedule[R, E, A]], fb: => OrF[Schedule[R, E, B]]): OrF[Schedule[R, E, (A, B)]] =
        OrF.wrap {
          OrF.unwrap(fa) || OrF.unwrap(fb)
        }
    }

  /**
   * The `CommutativeBoth` instance for `Vector`.
   */
  implicit def VectorCommutativeBoth: CommutativeBoth[Vector] =
    new CommutativeBoth[Vector] {
      def both[A, B](fa: => Vector[A], fb: => Vector[B]): Vector[(A, B)] = fa zip fb
    }

  /**
   * The `CommutativeBoth` instance for `ZIO`.
   */
  implicit def ZIOCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def both[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for failed `ZIO`.
   */
  implicit def ZIOFailureCommutativeBoth[R, A]: CommutativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new CommutativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zipPar Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `CommutativeBoth` instance for `ZLayer`.
   */
  implicit def ZLayerCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def both[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `ZManaged`.
   */
  implicit def ZManagedCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def both[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureCommutativeBoth[R, A]
    : CommutativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new CommutativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zipPar Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `CommutativeBoth` instance for `ZSink`.
   */
  implicit def ZSinkCommutativeBoth[R, E, I]: CommutativeBoth[({ type lambda[+a] = ZSink[R, E, I, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZSink[R, E, I, a] })#lambda] {
      def both[A, B](fa: => ZSink[R, E, I, A], fb: => ZSink[R, E, I, B]): ZSink[R, E, I, (A, B)] = fa zipPar fb
    }

  /**
   * The `CommutativeBoth` instance for `ZStream`.
   */
  implicit def ZStreamCommutativeBoth[R, E]: CommutativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new CommutativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def both[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, (A, B)] = fa zip fb
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
