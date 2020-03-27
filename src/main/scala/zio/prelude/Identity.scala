package zio.prelude

import zio.test.laws.Lawful

trait Identity[A] extends LeftIdentity[A] with RightIdentity[A] {
  def identity: A

  override final def leftIdentity: A = identity

  override final def rightIdentity: A = identity
}

object Identity extends Lawful[Identity with Equal] {

  final val laws = LeftIdentity.laws + RightIdentity.laws

  def apply[A](implicit Identity: Identity[A]): Identity[A] = Identity

  def fromFunctions[A](identity0: A, op: (A, A) => A): Identity[A] =
    new Identity[A] {
      def identity: A = identity0

      def combine(l: A, r: A): A = op(l, r)
    }

  implicit val charIdentity: Identity[Char] =
    Identity.fromFunctions[Char]('\u0000', (l: Char, r: Char) => (l + r).toChar)

  implicit val stringIdentity: Identity[String] =
    Identity.fromFunctions[String]("", (l: String, r: String) => l + r)

  implicit val byteIdentity: Identity[Byte] =
    Identity.fromFunctions[Byte](0, (l: Byte, r: Byte) => (l + r).toByte)

  implicit val multByteIdentity: Identity[MultByte] =
    Identity.fromFunctions[MultByte](MultByte(1), (l: MultByte, r: MultByte) => MultByte((l * r).toByte))

  implicit val intIdentity: Identity[Int] =
    Identity.fromFunctions[Int](0, (l: Int, r: Int) => l + r)

  implicit val multIntIdentity: Identity[MultInt] =
    Identity.fromFunctions[MultInt](MultInt(1), (l: MultInt, r: MultInt) => MultInt(l * r))

  implicit val longIdentity: Identity[Long] =
    Identity.fromFunctions[Long](0L, (l: Long, r: Long) => l + r)

  implicit val multLongIdentity: Identity[MultLong] =
    Identity.fromFunctions[MultLong](MultLong(1L), (l: MultLong, r: MultLong) => MultLong(l * r))

  implicit val floatIdentity: Identity[Float] =
    Identity.fromFunctions[Float](0, (l: Float, r: Float) => l + r)

  implicit val doubleIdentity: Identity[Double] =
    Identity.fromFunctions[Double](0, (l: Double, r: Double) => l + r)

  implicit val booleanIdentity: Identity[Boolean] =
    Identity.fromFunctions[Boolean](false, (l: Boolean, r: Boolean) => l || r)

  implicit val conjIdentity: Identity[Conj] =
    Identity.fromFunctions[Conj](Conj(true), (l: Conj, r: Conj) => Conj(l && r))

  implicit def optionIdentity[A: Associative]: Identity[Option[A]] =
    new Identity[Option[A]] {
      def identity: Option[A] = None

      def combine(l: Option[A], r: Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  implicit def eitherIdentity[E, A: Identity]: Identity[Either[E, A]] =
    new Identity[Either[E, A]] {
      def identity: Either[E, A] = Right(Identity[A].identity)

      def combine(l: Either[E, A], r: Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Left(l), _)         => Left(l)
          case (_, Left(r))         => Left(r)
          case (Right(l), Right(r)) => Right(l <> r)
        }
    }

  implicit def listIdentity[A]: Identity[List[A]] =
    Identity.fromFunctions[List[A]](Nil, _ ++ _)

  implicit def vectorIdentity[A]: Identity[Vector[A]] =
    Identity.fromFunctions[Vector[A]](Vector.empty, _ ++ _)

  implicit def mapIdentity[K, V: Associative]: Identity[Map[K, V]] =
    new Identity[Map[K, V]] {
      def identity: Map[K, V] = Map()

      def combine(l: Map[K, V], r: Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  implicit def setIdentity[A]: Identity[Set[A]] =
    Identity.fromFunctions[Set[A]](Set.empty, _ | _)

  implicit def tuple2Identity[A: Identity, B: Identity]: Identity[(A, B)] =
    new Identity[(A, B)] {
      def identity: (A, B) = (Identity[A].identity, Identity[B].identity)

      def combine(l: (A, B), r: (A, B)): (A, B) =
        (l._1 |+| r._1, l._2 |+| r._2)
    }

  implicit def tuple3Identity[A: Identity, B: Identity, C: Identity]: Identity[(A, B, C)] =
    new Identity[(A, B, C)] {
      def identity: (A, B, C) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity)

      def combine(l: (A, B, C), r: (A, B, C)): (A, B, C) =
        (l._1 |+| r._1, l._2 |+| r._2, l._3 |+| r._3)
    }

  implicit def tuple4Identity[A: Identity, B: Identity, C: Identity, D: Identity]: Identity[(A, B, C, D)] =
    new Identity[(A, B, C, D)] {
      def identity: (A, B, C, D) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity)

      def combine(l: (A, B, C, D), r: (A, B, C, D)): (A, B, C, D) =
        (l._1 |+| r._1, l._2 |+| r._2, l._3 |+| r._3, l._4 |+| r._4)
    }

  implicit def tuple5Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity]
    : Identity[(A, B, C, D, E)] =
    new Identity[(A, B, C, D, E)] {
      def identity: (A, B, C, D, E) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity, Identity[E].identity)

      def combine(l: (A, B, C, D, E), r: (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 |+| r._1, l._2 |+| r._2, l._3 |+| r._3, l._4 |+| r._4, l._5 |+| r._5)
    }

  implicit def tuple6Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity]
    : Identity[(A, B, C, D, E, F)] =
    new Identity[(A, B, C, D, E, F)] {
      def identity: (A, B, C, D, E, F) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity
        )

      def combine(l: (A, B, C, D, E, F), r: (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 |+| r._1, l._2 |+| r._2, l._3 |+| r._3, l._4 |+| r._4, l._5 |+| r._5, l._6 |+| r._6)
    }

  implicit def tuple7Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity, G: Identity]
    : Identity[(A, B, C, D, E, F, G)] =
    new Identity[(A, B, C, D, E, F, G)] {
      def identity: (A, B, C, D, E, F, G) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity
        )

      def combine(l: (A, B, C, D, E, F, G), r: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 |+| r._1, l._2 |+| r._2, l._3 |+| r._3, l._4 |+| r._4, l._5 |+| r._5, l._6 |+| r._6, l._7 |+| r._7)
    }

  implicit def tuple8Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity
  ]: Identity[(A, B, C, D, E, F, G, H)] =
    new Identity[(A, B, C, D, E, F, G, H)] {
      def identity: (A, B, C, D, E, F, G, H) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity
        )

      def combine(l: (A, B, C, D, E, F, G, H), r: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8
        )
    }

  implicit def tuple9Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I)] =
    new Identity[(A, B, C, D, E, F, G, H, I)] {
      def identity: (A, B, C, D, E, F, G, H, I) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity
        )

      def combine(l: (A, B, C, D, E, F, G, H, I), r: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9
        )
    }

  implicit def tuple10Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J)] {
      def identity: (A, B, C, D, E, F, G, H, I, J) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J),
        r: (A, B, C, D, E, F, G, H, I, J)
      ): (A, B, C, D, E, F, G, H, I, J) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10
        )
    }

  implicit def tuple11Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K),
        r: (A, B, C, D, E, F, G, H, I, J, K)
      ): (A, B, C, D, E, F, G, H, I, J, K) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11
        )
    }

  implicit def tuple12Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L),
        r: (A, B, C, D, E, F, G, H, I, J, K, L)
      ): (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12
        )
    }

  implicit def tuple13Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13
        )
    }

  implicit def tuple14Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14
        )
    }

  implicit def tuple15Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15
        )
    }

  implicit def tuple16Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16
        )
    }

  implicit def tuple17Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17
        )
    }

  implicit def tuple18Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17,
          l._18 |+| r._18
        )
    }

  implicit def tuple19Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17,
          l._18 |+| r._18,
          l._19 |+| r._19
        )
    }

  implicit def tuple20Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17,
          l._18 |+| r._18,
          l._19 |+| r._19,
          l._20 |+| r._20
        )
    }

  implicit def tuple21Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity,
          Identity[U].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17,
          l._18 |+| r._18,
          l._19 |+| r._19,
          l._20 |+| r._20,
          l._21 |+| r._21
        )
    }

  implicit def tuple22Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity,
    V: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity,
          Identity[U].identity,
          Identity[V].identity
        )

      def combine(
        l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V),
        r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          l._1 |+| r._1,
          l._2 |+| r._2,
          l._3 |+| r._3,
          l._4 |+| r._4,
          l._5 |+| r._5,
          l._6 |+| r._6,
          l._7 |+| r._7,
          l._8 |+| r._8,
          l._9 |+| r._9,
          l._10 |+| r._10,
          l._11 |+| r._11,
          l._12 |+| r._12,
          l._13 |+| r._13,
          l._14 |+| r._14,
          l._15 |+| r._15,
          l._16 |+| r._16,
          l._17 |+| r._17,
          l._18 |+| r._18,
          l._19 |+| r._19,
          l._20 |+| r._20,
          l._21 |+| r._21,
          l._22 |+| r._22
        )
    }
}

trait IdentitySyntax {
  implicit class IdentitySyntax[A](l: A) {
    def identity(implicit id: Identity[A]): A = id.identity

    def |+|(r: A)(implicit id: Identity[A]): A = id.combine(l, r)
  }
}
