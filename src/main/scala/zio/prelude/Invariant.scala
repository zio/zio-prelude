package zio.prelude

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

import zio.{ Chunk, ChunkBuilder }

trait Invariant[F[_]] {

  def invmap[A, B](f: A <=> B): F[A] <=> F[B]

  def identityLaw1[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    invmap(Equivalence.identity[A]).to(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A <=> B, g: B <=> C)(implicit equal: Equal[F[C]]): Boolean =
    (invmap(f) >>> invmap(g)).to(fa) === invmap(f andThen g).to(fa)

}

object Invariant /* extends InvariantVersionSpecific */ {

  def apply[F[_]](implicit invariant: Invariant[F]): Invariant[F] =
    invariant

  implicit val AssociativeInvariant: Invariant[Associative] =
    new Invariant[Associative] {
      def invmap[A, B](f: A <=> B): Associative[A] <=> Associative[B] =
        Equivalence(
          (a: Associative[A]) => Associative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Associative[B]) => Associative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `Traversable` instance for `Chunk`.
   */
  implicit val ChunkTraversable: Traversable[Chunk] =
    new Traversable[Chunk] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](chunk: Chunk[A])(f: A => G[B]): G[Chunk[B]] =
        chunk.foldLeft(ChunkBuilder.make[B]().succeed)((builder, a) => builder.zipWith(f(a))(_ += _)).map(_.result())
    }

  implicit val CommutativeInvariant: Invariant[Commutative] =
    new Invariant[Commutative] {
      def invmap[A, B](f: A <=> B): Commutative[A] <=> Commutative[B] =
        Equivalence(
          (a: Commutative[A]) => Commutative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Commutative[B]) => Commutative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  /**
   * The `Traversable` instance for `Either`.
   */
  implicit def EitherTraversable[E]: Traversable[({ type lambda[+a] = Either[E, a] })#lambda] =
    new Traversable[({ type lambda[+a] = Either[E, a] })#lambda] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](either: Either[E, A])(f: A => G[B]): G[Either[E, B]] =
        either.fold(Left(_).succeed, f(_).map(Right(_)))
    }

  implicit def FutureInvariant(implicit ec: ExecutionContext): Invariant[Future] =
    new Invariant[Future] {
      def invmap[A, B](f: A <=> B): Future[A] <=> Future[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val IdentityInvariant: Invariant[Identity] =
    new Invariant[Identity] {
      def invmap[A, B](f: A <=> B): Identity[A] <=> Identity[B] =
        Equivalence(
          (a: Identity[A]) => Identity.make[B](f.to(a.identity), (l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Identity[B]) => Identity.make[A](f.from(b.identity), (l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  implicit val InverseInvariant: Invariant[Inverse] =
    new Invariant[Inverse] {
      def invmap[A, B](f: A <=> B): Inverse[A] <=> Inverse[B] =
        Equivalence(
          (a: Inverse[A]) =>
            Inverse.make[B](
              f.to(a.identity),
              (l, r) => f.to(a.combine(f.from(l), f.from(r))),
              (l, r) => f.to(a.inverse(f.from(l), f.from(r)))
            ),
          (b: Inverse[B]) =>
            Inverse.make[A](
              f.from(b.identity),
              (l, r) => f.from(b.combine(f.to(l), f.to(r))),
              (l, r) => f.from(b.inverse(f.to(l), f.to(r)))
            )
        )
    }

  implicit def MapInvariant[V]: Invariant[({ type lambda[x] = Map[x, V] })#lambda] =
    new Invariant[({ type lambda[x] = Map[x, V] })#lambda] {
      def invmap[A, B](f: A <=> B): Map[A, V] <=> Map[B, V] =
        Equivalence(
          mapA => mapA.map { case (k, v) => (f.to(k), v) },
          mapB => mapB.map { case (k, v) => (f.from(k), v) }
        )
    }

  implicit val ListInvariant: Invariant[List] =
    new Invariant[List] {
      def invmap[A, B](f: A <=> B): List[A] <=> List[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val OptionInvariant: Invariant[Option] =
    new Invariant[Option] {
      def invmap[A, B](f: A <=> B): Option[A] <=> Option[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val SetInvariant: Invariant[Set] =
    new Invariant[Set] {
      def invmap[A, B](f: A <=> B): Set[A] <=> Set[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }

  implicit val TryInvariant: Invariant[Try] =
    new Invariant[Try] {
      def invmap[A, B](f: A <=> B): Try[A] <=> Try[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val VectorInvariant: Invariant[Vector] =
    new Invariant[Vector] {
      def invmap[A, B](f: A <=> B): Vector[A] <=> Vector[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }
}
