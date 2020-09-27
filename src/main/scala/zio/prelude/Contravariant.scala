package zio.prelude

import zio.prelude.coherent.ContravariantDeriveEqual
import zio.stream.{ ZSink, ZStream }
import zio.test.TestResult
import zio.test.laws._
import zio.{ Schedule, ZIO, ZLayer, ZManaged, ZQueue, ZRef, ZRefM }

trait ContravariantSubset[F[-_], Subset[_]] {
  def contramapSubset[A, B: Subset](f: B => A): F[A] => F[B]
}

/**
 * `Contravariant[F]` provides implicit evidence that `F[-_]` is a
 * contravariant endofunctor in the category of Scala objects.
 *
 * `Contravariant` instances of type `F[A]` "consume" values of type `A` in
 * some sense. For example, `Equal[A]` takes two values of type `A` as input
 * and returns a `Boolean` indicating whether they are equal. Similarly, a
 * `Ord[A]` takes two values of type `A` as input and returns an `Ordering`
 * with the result of comparing them and `Hash` takes an `A` value and returns
 * an `Int`.
 *
 * Common examples of contravariant instances in ZIO include effects with
 * regard to their environment types, sinks with regard to their input type,
 * and polymorphic queues and references regarding their input types.
 *
 * `Contravariant` instances support a `contramap` operation, which allows
 * transforming the input type given a function from the new input type to the
 * old input type. For example, if we have an `Ord[Int]` that allows us to
 * compare two integers and we have a function `String => Int` that returns
 * the length of a string, then we can construct an `Ord[String]` that
 * compares strings by computing their lengths with the provided function and
 * comparing those.
 */
trait Contravariant[F[-_]] extends ContravariantSubset[F, AnyType] with Invariant[F]      {
  final def contramapSubset[A, B: AnyType](f: B => A): F[A] => F[B] =
    contramap(f)

  /**
   * Lift a function from `B` to `A` to a function from `F[A]` to `F[B]`.
   */
  def contramap[A, B](f: B => A): F[A] => F[B]

  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => contramap(f.from)(fa), (fb: F[B]) => contramap(f.to)(fb))
}
object Contravariant       extends LawfulF.Contravariant[ContravariantDeriveEqual, Equal] {

  /**
   * Contramapping with the identity function must not change the structure.
   */
  val identityLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.Law1[ContravariantDeriveEqual, Equal]("identityLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.contramap(identity[A]) <-> fa
    }

  /**
   * Contramapping by `f` followed by `g` must be the same as contramapping
   * with the composition of `f` and `g`.
   */
  val compositionLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.ComposeLaw[ContravariantDeriveEqual, Equal]("compositionLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        f: B => A,
        g: C => B
      ): TestResult = {
        // Dotty can't infer this https://github.com/zio/zio-prelude/issues/273
        implicit val equalFC: Equal[F[C]] = Equal.DeriveEqual[F, C]
        fa.contramap(f).contramap(g) <-> fa.contramap(f compose g)
      }
    }

  /**
   * The set of all laws that instances of `Contravariant` must satisfy.
   */
  val laws: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    identityLaw + compositionLaw

  /**
   * Summons an implicit `Contravariant[F]`.
   */
  def apply[F[-_]](implicit contravariant: Contravariant[F]): Contravariant[F] =
    contravariant

  /**
   * The contravariant instance for `Schedule`.
   */
  implicit def ScheduleContravariant[R, B]: Contravariant[({ type lambda[-x] = Schedule[R, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = Schedule[R, x, B] })#lambda] {
      def contramap[A, A0](f: A0 => A): Schedule[R, A, B] => Schedule[R, A0, B] =
        schedule => schedule.contramap(f)
    }

  /**
   * The contravariant instance for `ZIO`.
   */
  implicit def ZIOContravariant[E, A]: Contravariant[({ type lambda[-x] = ZIO[x, E, A] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZIO[x, E, A] })#lambda] {
      def contramap[R, R0](f: R0 => R): ZIO[R, E, A] => ZIO[R0, E, A] =
        zio => zio.provideSome(f)
    }

  /**
   * The contravariant instance for `ZLayer`.
   */
  implicit def ZLayerContravariant[E, ROut]: Contravariant[({ type lambda[-x] = ZLayer[x, E, ROut] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZLayer[x, E, ROut] })#lambda] {
      def contramap[RIn, RIn0](f: RIn0 => RIn): ZLayer[RIn, E, ROut] => ZLayer[RIn0, E, ROut] =
        layer => ZLayer.fromFunctionMany(f) >>> layer
    }

  /**
   * The contravariant instance for `ZManaged`.
   */
  implicit def ZManagedContravariant[E, A]: Contravariant[({ type lambda[-x] = ZManaged[x, E, A] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZManaged[x, E, A] })#lambda] {
      def contramap[R, R0](f: R0 => R): ZManaged[R, E, A] => ZManaged[R0, E, A] =
        managed => managed.provideSome(f)
    }

  /**
   * The contravariant instance for `ZQueue`.
   */
  implicit def ZQueueContravariant[RA, EA, RB, EB, B]
    : Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZQueue[RA, EA, RB, EB, x, B] })#lambda] {
      def contramap[A, C](f: C => A): ZQueue[RA, EA, RB, EB, A, B] => ZQueue[RA, EA, RB, EB, C, B] =
        queue => queue.contramap(f)
    }

  /**
   * The contravariant instance for `ZRef`.
   */
  implicit def ZRefContravariant[EA, EB, B]: Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRef[EA, EB, x, B] })#lambda] {
      def contramap[A, C](f: C => A): ZRef[EA, EB, A, B] => ZRef[EA, EB, C, B] =
        ref => ref.contramap(f)
    }

  /**
   * The contravariant instance for `ZRefM`.
   */
  implicit def ZRefMContravariant[RA, RB, EA, EB, B]
    : Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZRefM[RA, RB, EA, EB, x, B] })#lambda] {
      def contramap[A, C](f: C => A): ZRefM[RA, RB, EA, EB, A, B] => ZRefM[RA, RB, EA, EB, C, B] =
        ref => ref.contramap(f)
    }

  /**
   * The contravariant instance for `ZSink`.
   */
  implicit def ZSinkContravariant[R, E, L, Z]: Contravariant[({ type lambda[-x] = ZSink[R, E, x, L, Z] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZSink[R, E, x, L, Z] })#lambda] {
      def contramap[A, C](f: C => A): ZSink[R, E, A, L, Z] => ZSink[R, E, C, L, Z] =
        sink => sink.contramap(f)
    }

  /**
   * The contravariant instance for `ZStream`.
   */
  implicit def ZStreamContravariant[E, A]: Contravariant[({ type lambda[-x] = ZStream[x, E, A] })#lambda] =
    new Contravariant[({ type lambda[-x] = ZStream[x, E, A] })#lambda] {
      def contramap[R, R0](f: R0 => R): ZStream[R, E, A] => ZStream[R0, E, A] =
        stream => stream.provideSome(f)
    }
}

trait ContravariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class ContravariantOps[F[-_], A](private val self: F[A]) {
    def contramap[B](f: B => A)(implicit contravariant: Contravariant[F]): F[B] =
      contravariant.contramap(f)(self)
  }
}
