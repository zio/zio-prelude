package zio.prelude

import zio.prelude.coherent.CommutativeEqual
import zio.prelude.newtypes.{ And, Max, Min, Or, Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Commutative[A] extends Associative[A] {
  self =>
  final def commute: Commutative[A] = Commutative((l, r) => self.combine(r, l))
}

object Commutative extends Lawful[CommutativeEqual] {

  val commutativeLaw: Laws[CommutativeEqual] =
    new Laws.Law2[CommutativeEqual]("commutativeLaw") {
      def apply[A: CommutativeEqual](a1: A, a2: A): TestResult =
        (a1 <> a2) <-> (a2 <> a1)
    }

  val laws: Laws[CommutativeEqual] =
    commutativeLaw + Closure.laws

  def apply[A](implicit commutative: Commutative[A]): Commutative[A] = commutative

  def make[A](f: (A, A) => A): Commutative[A] =
    (l, r) => f(l, r)

  implicit val BooleanConjunctionCommutative: Commutative[And] = Commutative.make((l, r) => And(l && r))

  implicit val BooleanDisjunctionCommutative: Commutative[Or] = Commutative.make((l, r) => Or(l || r))

  implicit val BooleanProdCommutative: Commutative[Prod[Boolean]] =
    Commutative.make((l, r) => Prod(l && r))

  implicit val BooleanSumCommutative: Commutative[Sum[Boolean]] =
    Commutative.make((l, r) => Sum(l || r))

  implicit val ByteProdCommutative: Commutative[Prod[Byte]] =
    Commutative.make((l, r) => Prod((l * r).toByte))

  implicit val ByteSumCommutative: Commutative[Sum[Byte]] =
    Commutative.make((l, r) => Sum((l + r).toByte))

  implicit val CharProdCommutative: Commutative[Prod[Char]] = Commutative.make((l, r) => Prod((l * r).toChar))

  implicit val CharSumCommutative: Commutative[Sum[Char]] = Commutative.make((l, r) => Sum((l + r).toChar))

  implicit val DoubleProdCommutative: Commutative[Prod[Double]] =
    Commutative.make((l, r) => Prod(l * r))

  implicit val DoubleSumCommutative: Commutative[Sum[Double]] =
    Commutative.make((l, r) => Sum(l + r))

  implicit def EitherCommutative[E: Commutative, A: Commutative]: Commutative[Either[E, A]] =
    new Commutative[Either[E, A]] {
      def combine(l: => Either[E, A], r: => Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Right(l), Right(r)) => Right(l <> r)
          case (Left(l), Right(_))  => Left(l)
          case (Right(_), Left(r))  => Left(r)
          case (Left(l), Left(r))   => Left(l <> r)
        }
    }

  implicit val FloatProdCommutative: Commutative[Prod[Float]] =
    Commutative.make((l, r) => Prod(l * r))

  implicit val FloatSumCommutative: Commutative[Sum[Float]] =
    Commutative.make((l, r) => Sum(l + r))

  implicit val IntProdCommutative: Commutative[Prod[Int]] =
    Commutative.make((l, r) => Prod(l * r))

  implicit val IntSumCommutative: Commutative[Sum[Int]] =
    Commutative.make((l, r) => Sum(l + r))

  implicit val LongProdCommutative: Commutative[Prod[Long]] =
    Commutative.make((l, r) => Prod(l * r))

  implicit val LongSumCommutative: Commutative[Sum[Long]] =
    Commutative.make((l, r) => Sum(l + r))

  implicit def MapCommutative[K, V: Commutative]: Commutative[Map[K, V]] =
    new Commutative[Map[K, V]] {

      def combine(l: => Map[K, V], r: => Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  implicit def MaxCommutative[A: Ord]: Commutative[Max[A]] =
    make((l: Max[A], r: Max[A]) => if (l >= r) l else r)

  implicit def MinCommutative[A: Ord]: Commutative[Min[A]] =
    make((l: Min[A], r: Min[A]) => if (l <= r) l else r)

  implicit def OptionCommutative[A: Commutative]: Commutative[Option[A]] =
    new Commutative[Option[A]] {
      def combine(l: => Option[A], r: => Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  implicit def SetCommutative[A]: Commutative[Set[A]] = Commutative.make(_ | _)

  implicit val ShortProdCommutative: Commutative[Prod[Short]] =
    Commutative.make((l, r) => Prod((l * r).toShort))

  implicit val ShortSumCommutative: Commutative[Sum[Short]] =
    Commutative.make((l, r) => Sum((l + r).toShort))

  implicit def Tuple2Commutative[A: Commutative, B: Commutative]: Commutative[(A, B)] =
    new Commutative[(A, B)] {
      def combine(l: => (A, B), r: => (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  implicit def Tuple3Commutative[A: Commutative, B: Commutative, C: Commutative]: Commutative[(A, B, C)] =
    new Commutative[(A, B, C)] {
      def combine(l: => (A, B, C), r: => (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  implicit def Tuple4Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative]
    : Commutative[(A, B, C, D)] =
    new Commutative[(A, B, C, D)] {
      def combine(l: => (A, B, C, D), r: => (A, B, C, D)): (A, B, C, D) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
    }

  implicit def Tuple5Commutative[A: Commutative, B: Commutative, C: Commutative, D: Commutative, E: Commutative]
    : Commutative[(A, B, C, D, E)] =
    new Commutative[(A, B, C, D, E)] {
      def combine(l: => (A, B, C, D, E), r: => (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5)
    }

  implicit def Tuple6Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative
  ]: Commutative[(A, B, C, D, E, F)] =
    new Commutative[(A, B, C, D, E, F)] {
      def combine(l: => (A, B, C, D, E, F), r: => (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6)
    }

  implicit def Tuple7Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative
  ]: Commutative[(A, B, C, D, E, F, G)] =
    new Commutative[(A, B, C, D, E, F, G)] {
      def combine(l: => (A, B, C, D, E, F, G), r: => (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7)
    }

  implicit def Tuple8Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H)] =
    new Commutative[(A, B, C, D, E, F, G, H)] {
      def combine(l: => (A, B, C, D, E, F, G, H), r: => (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7, l._8 <> r._8)
    }

  implicit def Tuple9Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I)] =
    new Commutative[(A, B, C, D, E, F, G, H, I)] {
      def combine(l: => (A, B, C, D, E, F, G, H, I), r: => (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9
        )
    }

  implicit def Tuple10Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J),
        r: => (A, B, C, D, E, F, G, H, I, J)
      ): (A, B, C, D, E, F, G, H, I, J) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10
        )
    }

  implicit def Tuple11Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K),
        r: => (A, B, C, D, E, F, G, H, I, J, K)
      ): (A, B, C, D, E, F, G, H, I, J, K) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11
        )
    }

  implicit def Tuple12Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L)
      ): (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12
        )
    }

  implicit def Tuple13Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13
        )
    }

  implicit def Tuple14Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14
        )
    }

  implicit def Tuple15Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15
        )
    }

  implicit def Tuple16Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16
        )
    }

  implicit def Tuple17Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17
        )
    }

  implicit def Tuple18Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18
        )
    }

  implicit def Tuple19Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19
        )
    }

  implicit def Tuple20Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20
        )
    }

  implicit def Tuple21Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative,
    U: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21
        )
    }

  implicit def Tuple22Commutative[
    A: Commutative,
    B: Commutative,
    C: Commutative,
    D: Commutative,
    E: Commutative,
    F: Commutative,
    G: Commutative,
    H: Commutative,
    I: Commutative,
    J: Commutative,
    K: Commutative,
    L: Commutative,
    M: Commutative,
    N: Commutative,
    O: Commutative,
    P: Commutative,
    Q: Commutative,
    R: Commutative,
    S: Commutative,
    T: Commutative,
    U: Commutative,
    V: Commutative
  ]: Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Commutative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21,
          l._22 <> r._22
        )
    }
}
