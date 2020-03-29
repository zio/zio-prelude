package zio.prelude

import zio.prelude.newtypes.{And, Or, Prod, Sum}
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait Closure[A] {
  def combine(l: A, r: A): A
}

object Closure extends Lawful[Closure] {

  final val closureLaw = new Laws.Law2[Closure]("closureLaw") {
    def apply[A: Closure](a1: A, a2: A): TestResult =
      (try {
        (a1 <> a2) != null
      } catch { case _: Throwable => false }) <-> true
  }

  final val laws = closureLaw

  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def make[A](f: (A, A) => A): Closure[A] =
    new Closure[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

  implicit val CharClosure: Closure[Char] =
    Closure.make[Char]( (l: Char, r: Char) => (l + r).toChar)

  implicit val StringClosure: Closure[String] =
    Closure.make[String]( (l: String, r: String) => l + r)

  implicit val ByteSumClosure: Closure[Sum[Byte]] =
    Closure.make[Sum[Byte]]((l: Sum[Byte], r: Sum[Byte]) => Sum((l + r).toByte))

  implicit val ByteProdClosure: Closure[Prod[Byte]] =
    Closure.make[Prod[Byte]]( (l: Prod[Byte], r: Prod[Byte]) => Prod((l * r).toByte))

  implicit val ShortSumClosure: Closure[Sum[Short]] =
    Closure.make[Sum[Short]]((l: Sum[Short], r: Sum[Short]) => Sum((l + r).toShort))

  implicit val ShortProdClosure: Closure[Prod[Short]] =
    Closure.make[Prod[Short]]((l: Prod[Short], r: Prod[Short]) => Prod((l * r).toShort))

  implicit val IntSumClosure: Closure[Sum[Int]] =
    Closure.make[Sum[Int]]((l: Sum[Int], r: Sum[Int]) => Sum(l + r))

  implicit val IntProdClosure: Closure[Prod[Int]] =
    Closure.make[Prod[Int]]((l: Prod[Int], r: Prod[Int]) => Prod(l * r))

  implicit val LongSumClosure: Closure[Sum[Long]] =
    Closure.make[Sum[Long]]((l: Sum[Long], r: Sum[Long]) => Sum(l + r))

  implicit val LongProdClosure: Closure[Prod[Long]] =
    Closure.make[Prod[Long]]((l: Prod[Long], r: Prod[Long]) => Prod(l * r))

  implicit val FloatSumClosure: Closure[Sum[Float]] =
    Closure.make[Sum[Float]]((l: Sum[Float], r: Sum[Float]) => Sum(l + r))

  implicit val FloatProdClosure: Closure[Prod[Float]] =
    Closure.make[Prod[Float]]((l: Prod[Float], r: Prod[Float]) => Prod(l * r))

  implicit val DoubleSumClosure: Closure[Sum[Double]] =
    Closure.make[Sum[Double]]((l: Sum[Double], r: Sum[Double]) => Sum(l + r))

  implicit val DoubleProdClosure: Closure[Prod[Double]] =
    Closure.make[Prod[Double]]((l: Prod[Double], r: Prod[Double]) => Prod(l * r))

  implicit val BooleanDisjunctionClosure: Closure[Or] =
    Closure.make[Or]((l: Or, r: Or) => Or(l || r))

  implicit val BooleanConjunctionClosure: Closure[And] =
    Closure.make[And]((l: And, r: And) => And(l && r))

  implicit def OptionClosure[A: Closure]: Closure[Option[A]] =
    new Closure[Option[A]] {
      def combine(l: Option[A], r: Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  // TODO - Confirm
  implicit def EitherClosure[E, A: Closure]: Closure[Either[E, A]] =
    new Closure[Either[E, A]] {
      def combine(l: Either[E, A], r: Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Left(l), _)         => Left(l)
          case (_, Left(r))         => Left(r)
          case (Right(l), Right(r)) => Right(l <> r)
        }
    }

  implicit def ListClosure[A]: Closure[List[A]] =
    Closure.make[List[A]]((l: List[A], r: List[A]) => l ++ r)

  implicit def VectorClosure[A]: Closure[Vector[A]] =
    Closure.make[Vector[A]]((l: Vector[A], r: Vector[A]) =>  l ++ r)

  implicit def MapClosure[K, V: Closure]: Closure[Map[K, V]] =
    new Closure[Map[K, V]] {

      def combine(l: Map[K, V], r: Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  implicit def SetClosure[A]: Closure[Set[A]] =
    Closure.make[Set[A]]((l: Set[A], r: Set[A]) => l | r)

  implicit def Tuple2Closure[A: Closure, B: Closure]: Closure[(A, B)] =
    new Closure[(A, B)] {
      def combine(l: (A, B), r: (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  implicit def Tuple3Closure[A: Closure, B: Closure, C: Closure]: Closure[(A, B, C)] =
    new Closure[(A, B, C)] {
      def combine(l: (A, B, C), r: (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  implicit def Tuple4Closure[A: Closure, B: Closure, C: Closure, D: Closure]: Closure[(A, B, C, D)] =
    new Closure[(A, B, C, D)] {
      def combine(l: (A, B, C, D), r: (A, B, C, D)): (A, B, C, D) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
    }

  implicit def Tuple5Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure]
  : Closure[(A, B, C, D, E)] =
    new Closure[(A, B, C, D, E)] {

      def combine(l: (A, B, C, D, E), r: (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5)
    }

  implicit def Tuple6Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure, F: Closure]
  : Closure[(A, B, C, D, E, F)] =
    new Closure[(A, B, C, D, E, F)] {
      def combine(l: (A, B, C, D, E, F), r: (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6)
    }

  implicit def Tuple7Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure, F: Closure, G: Closure]
  : Closure[(A, B, C, D, E, F, G)] =
    new Closure[(A, B, C, D, E, F, G)] {
      def combine(l: (A, B, C, D, E, F, G), r: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7)
    }

  implicit def Tuple8Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure
  ]: Closure[(A, B, C, D, E, F, G, H)] =
    new Closure[(A, B, C, D, E, F, G, H)] {

      def combine(l: (A, B, C, D, E, F, G, H), r: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8
        )
    }

  implicit def Tuple9Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I)] =
    new Closure[(A, B, C, D, E, F, G, H, I)] {

      def combine(l: (A, B, C, D, E, F, G, H, I), r: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) =
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

  implicit def Tuple10Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J),
                   r: (A, B, C, D, E, F, G, H, I, J)
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

  implicit def Tuple11Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K),
                   r: (A, B, C, D, E, F, G, H, I, J, K)
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

  implicit def Tuple12Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L)
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

  implicit def Tuple13Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M)
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

  implicit def Tuple14Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
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

  implicit def Tuple15Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
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

  implicit def Tuple16Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
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

  implicit def Tuple17Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
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

  implicit def Tuple18Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure,
    R: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
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

  implicit def Tuple19Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure,
    R: Closure,
    S: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
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

  implicit def Tuple20Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure,
    R: Closure,
    S: Closure,
    T: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
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

  implicit def Tuple21Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure,
    R: Closure,
    S: Closure,
    T: Closure,
    U: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
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

  implicit def Tuple22Closure[
    A: Closure,
    B: Closure,
    C: Closure,
    D: Closure,
    E: Closure,
    F: Closure,
    G: Closure,
    H: Closure,
    I: Closure,
    J: Closure,
    K: Closure,
    L: Closure,
    M: Closure,
    N: Closure,
    O: Closure,
    P: Closure,
    Q: Closure,
    R: Closure,
    S: Closure,
    T: Closure,
    U: Closure,
    V: Closure
  ]: Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Closure[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {

      def combine(
                   l: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V),
                   r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
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


trait ClosureSyntax {

  implicit class ClosureSyntax[A](l: A) {
    def combine(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)

    def <>(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)
  }

}
