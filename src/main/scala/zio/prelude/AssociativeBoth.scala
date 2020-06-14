package zio.prelude

import zio._
import zio.prelude.coherent.AssociativeBothEqualFInvariant
import zio.prelude.newtypes.Failure
import zio.stm.ZSTM
import zio.stream.{ ZSink, ZStream, ZTransducer }
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.Try

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
  val associativityLaw: LawsF.Invariant[AssociativeBothEqualFInvariant, Equal] =
    new LawsF.Invariant.Law3[AssociativeBothEqualFInvariant, Equal]("associativityLaw") {
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
  val laws: LawsF.Invariant[AssociativeBothEqualFInvariant, Equal] =
    associativityLaw

  /**
   * Summons an implicit `AssociativeBoth[F]`.
   */
  def apply[F[+_]](implicit associativeBoth: AssociativeBoth[F]): AssociativeBoth[F] =
    associativeBoth

  /**
   * The `AssociativeBoth` instance for `Chunk`.
   */
  implicit def ChunkAssociativeBoth: AssociativeBoth[Chunk] =
    new AssociativeBoth[Chunk] {
      def both[A, B](fa: => Chunk[A], fb: => Chunk[B]): Chunk[(A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Either`.
   */
  implicit def EitherAssociativeBoth[L]: AssociativeBoth[({ type lambda[+r] = Either[L, r] })#lambda] =
    new AssociativeBoth[({ type lambda[+r] = Either[L, r] })#lambda] {
      def both[A, B](fa: => Either[L, A], fb: => Either[L, B]): Either[L, (A, B)] =
        fa.right.flatMap(a => fb.right.map(b => (a, b)))
    }

  /**
   * The `AssociativeBoth` instance for a failed `Either`
   */
  implicit def EitherFailedAssociativeBoth[R]: AssociativeBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new AssociativeBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      def both[A, B](fa: => Failure[Either[A, R]], fb: => Failure[Either[B, R]]): Failure[Either[(A, B), R]] =
        Failure.wrap {
          Failure
            .unwrap(fa)
            .left
            .flatMap(a => Failure.unwrap(fb).left.map(b => (a, b)))
        }
    }

  /**
   * The `AssociativeBoth` instance for `Exit`.
   */
  implicit def ExitAssociativeBoth[E]: AssociativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def both[A, B](fa: => Exit[E, A], fb: => Exit[E, B]): Exit[E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Fiber`.
   */
  implicit def FiberAssociativeBoth[E]: AssociativeBoth[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def both[A, B](fa: => Fiber[E, A], fb: => Fiber[E, B]): Fiber[E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Fiber`.
   */
  implicit def FutureAssociativeBoth: AssociativeBoth[Future] =
    new AssociativeBoth[Future] {
      def both[A, B](fa: => Future[A], fb: => Future[B]): Future[(A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Id`.
   */
  implicit val IdAssociativeBoth: AssociativeBoth[Id] =
    new AssociativeBoth[Id] {
      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] = Id((Id.unwrap(fa), Id.unwrap(fb)))
    }

  /**
   * The `AssociativeBoth` instance for `List`.
   */
  implicit def ListAssociativeBoth: AssociativeBoth[List] =
    new AssociativeBoth[List] {
      def both[A, B](fa: => List[A], fb: => List[B]): List[(A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `NonEmptyChunk`.
   */
  implicit def NonEmptyChunkAssociativeBoth: AssociativeBoth[NonEmptyChunk] =
    new AssociativeBoth[NonEmptyChunk] {
      def both[A, B](fa: => NonEmptyChunk[A], fb: => NonEmptyChunk[B]): NonEmptyChunk[(A, B)] =
        (fa zipWith fb)((_, _))
    }

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
   * The `AssociativeBoth` instance for `Schedule`.
   */
  implicit def ScheduleAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = Schedule[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = Schedule[R, E, a] })#lambda] {
      def both[A, B](fa: => Schedule[R, E, A], fb: => Schedule[R, E, B]): Schedule[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Try`.
   */
  implicit def TryAssociativeBoth[R, E]: AssociativeBoth[Try] =
    new AssociativeBoth[Try] {
      def both[A, B](fa: => Try[A], fb: => Try[B]): Try[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `AssociativeBoth` instance for `Vector`.
   */
  implicit def VectorAssociativeBoth: AssociativeBoth[Vector] =
    new AssociativeBoth[Vector] {
      def both[A, B](fa: => Vector[A], fb: => Vector[B]): Vector[(A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `ZIO`.
   */
  implicit def ZIOAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def both[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for failed `ZIO`.
   */
  implicit def ZIOFailureAssociativeBoth[R, A]: AssociativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new AssociativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zip Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `AssociativeBoth` instance for `ZLayer`.
   */
  // TODO
  implicit def ZLayerAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def both[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, (A, B)] = ???
    }

  /**
   * The `AssociativeBoth` instance for `ZManaged`.
   */
  implicit def ZManagedAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def both[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureAssociativeBoth[R, A]
    : AssociativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new AssociativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zip Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `AssociativeBoth` instance for `ZSink`.
   */
  implicit def ZSinkAssociativeBoth[R, E, I]: AssociativeBoth[({ type lambda[+a] = ZSink[R, E, I, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZSink[R, E, I, a] })#lambda] {
      def both[A, B](fa: => ZSink[R, E, I, A], fb: => ZSink[R, E, I, B]): ZSink[R, E, I, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `ZSTM`.
   */
  implicit def ZSTMAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] {
      def both[A, B](fa: => ZSTM[R, E, A], fb: => ZSTM[R, E, B]): ZSTM[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `ZStream`.
   */
  implicit def ZStreamAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def both[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for failed `ZStream`.
   */
  // TODO
  implicit def ZStreamFailureAssociativBoth[R, A]
    : AssociativeBoth[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] =
    new AssociativeBoth[({ type lambda[+e] = Failure[ZStream[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZStream[R, EA, A]],
        fb: => Failure[ZStream[R, EB, A]]
      ): Failure[ZStream[R, (EA, EB), A]] = ???

    }

  /**
   * The `AssociativeBoth` instance for `ZTransducer`.
   */
  // TODO
  implicit def ZTransducerAssociativeBoth[R, E, I]
    : AssociativeBoth[({ type lambda[+a] = ZTransducer[R, E, I, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZTransducer[R, E, I, a] })#lambda] {
      def both[A, B](fa: => ZTransducer[R, E, I, A], fb: => ZTransducer[R, E, I, B]): ZTransducer[R, E, I, (A, B)] =
        ???
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
