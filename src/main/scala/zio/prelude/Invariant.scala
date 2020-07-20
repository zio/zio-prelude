package zio.prelude

import zio.Chunk

trait Invariant[F[_]] {

  def invmap[A, B](f: A <=> B): F[A] <=> F[B]

  def identityLaw1[A](fa: F[A])(implicit equal: Equal[F[A]]): Boolean =
    invmap(Equivalence.identity[A]).to(fa) === fa

  def compositionLaw[A, B, C](fa: F[A], f: A <=> B, g: B <=> C)(implicit equal: Equal[F[C]]): Boolean =
    (invmap(f) >>> invmap(g)).to(fa) === invmap(f andThen g).to(fa)

}

object Invariant {

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

  implicit val ChunkInvariant: Invariant[Chunk] =
    new Invariant[Chunk] {
      def invmap[A, B](f: A <=> B): Chunk[A] <=> Chunk[B] =
        Equivalence(_.map(f.to), _.map(f.from))
    }

  implicit val ClosureInvariant: Invariant[Closure] =
    new Invariant[Closure] {
      def invmap[A, B](f: A <=> B): Closure[A] <=> Closure[B] =
        Equivalence(
          (a: Closure[A]) => Closure.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Closure[B]) => Closure.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  implicit val CommutativeInvariant: Invariant[Commutative] =
    new Invariant[Commutative] {
      def invmap[A, B](f: A <=> B): Commutative[A] <=> Commutative[B] =
        Equivalence(
          (a: Commutative[A]) => Commutative.make[B]((l, r) => f.to(a.combine(f.from(l), f.from(r)))),
          (b: Commutative[B]) => Commutative.make[A]((l, r) => f.from(b.combine(f.to(l), f.to(r))))
        )
    }

  implicit def EitherInvariant[L]: Invariant[({ type lambda[r] = Either[L, r] })#lambda] =
    new Invariant[({ type lambda[r] = Either[L, r] })#lambda] {
      def invmap[A, B](f: A <=> B): Either[L, A] <=> Either[L, B] =
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

  implicit val VectorInvariant: Invariant[Vector] =
    new Invariant[Vector] {
      def invmap[A, B](f: A <=> B): Vector[A] <=> Vector[B] =
        Equivalence(setA => setA.map(f.to), setB => setB.map(f.from))
    }
}
