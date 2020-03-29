package zio.prelude

import zio.prelude.coherent.AssociativeEqual
import zio.prelude.newtypes.{And, First, Last, Max, Min, Or, Prod, Sum}
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

/**
 * The `Associative[A]` type class describes an associative binary operator
 * for a type `A`. For example, addition for integers, and string
 * concatenation for strings.
 */
trait Associative[A] extends Closure[A]

object Associative extends Lawful[Associative with Equal] with AssociativeEqual {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  final val associativityLaw = new Laws.Law3[Associative with Equal]("associativityLaw") {
    def apply[A](a1: A, a2: A, a3: A)(implicit A: Associative[A] with Equal[A]): TestResult =
      (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
  }

  final val laws = associativityLaw + Closure.laws

  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  def make[A](f: (A, A) => A): Associative[A] =
    new Associative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

  implicit def lastAssociative[A]: Associative[Last[A]] =
    make((_: Last[A], r: Last[A]) => r)

  implicit def firstAssociative[A]: Associative[First[A]] =
    make((l: First[A], _: First[A]) => l)

  implicit def minAssociative[A: Ord]: Associative[Min[A]] =
    make((l: Min[A], r: Min[A]) => if (l < r) l else r)

  implicit def maxAssociative[A: Ord]: Associative[Max[A]] =
    make((l: Max[A], r: Max[A]) => if (l > r) l else r)

  implicit val CharAssociative: Associative[Char] =
    Associative.make[Char]( (l: Char, r: Char) => (l + r).toChar)

  implicit val StringAssociative: Associative[String] =
    Associative.make[String]( (l: String, r: String) => l + r)

  implicit val ByteSumAssociative: Associative[Sum[Byte]] =
    Associative.make[Sum[Byte]]((l: Sum[Byte], r: Sum[Byte]) => Sum((l + r).toByte))

  implicit val ByteProdAssociative: Associative[Prod[Byte]] =
    Associative.make[Prod[Byte]]( (l: Prod[Byte], r: Prod[Byte]) => Prod((l * r).toByte))

  implicit val ShortSumAssociative: Associative[Sum[Short]] =
    Associative.make[Sum[Short]]((l: Sum[Short], r: Sum[Short]) => Sum((l + r).toShort))

  implicit val ShortProdAssociative: Associative[Prod[Short]] =
    Associative.make[Prod[Short]]((l: Prod[Short], r: Prod[Short]) => Prod((l * r).toShort))

  implicit val IntSumAssociative: Associative[Sum[Int]] =
    Associative.make[Sum[Int]]((l: Sum[Int], r: Sum[Int]) => Sum(l + r))

  implicit val IntProdAssociative: Associative[Prod[Int]] =
    Associative.make[Prod[Int]]((l: Prod[Int], r: Prod[Int]) => Prod(l * r))

  implicit val LongSumAssociative: Associative[Sum[Long]] =
    Associative.make[Sum[Long]]((l: Sum[Long], r: Sum[Long]) => Sum(l + r))

  implicit val LongProdAssociative: Associative[Prod[Long]] =
    Associative.make[Prod[Long]]((l: Prod[Long], r: Prod[Long]) => Prod(l * r))

  implicit val FloatSumAssociative: Associative[Sum[Float]] =
    Associative.make[Sum[Float]]((l: Sum[Float], r: Sum[Float]) => Sum(l + r))

  implicit val FloatProdAssociative: Associative[Prod[Float]] =
    Associative.make[Prod[Float]]((l: Prod[Float], r: Prod[Float]) => Prod(l * r))

  implicit val DoubleSumAssociative: Associative[Sum[Double]] =
    Associative.make[Sum[Double]]((l: Sum[Double], r: Sum[Double]) => Sum(l + r))

  implicit val DoubleProdAssociative: Associative[Prod[Double]] =
    Associative.make[Prod[Double]]((l: Prod[Double], r: Prod[Double]) => Prod(l * r))

  implicit val BooleanDisjunctionAssociative: Associative[Or] =
    Associative.make[Or]((l: Or, r: Or) => Or(l || r))

  implicit val BooleanConjunctionAssociative: Associative[And] =
    Associative.make[And]((l: And, r: And) => And(l && r))

  implicit def OptionAssociative[A: Associative]: Associative[Option[A]] =
    new Associative[Option[A]] {
      def combine(l: Option[A], r: Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  implicit def EitherAssociative[E, A: Associative]: Associative[Either[E, A]] =
    new Associative[Either[E, A]] {
      def combine(l: Either[E, A], r: Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Left(l), _)         => Left(l)
          case (_, Left(r))         => Left(r)
          case (Right(l), Right(r)) => Right(l <> r)
        }
    }

  implicit def ListAssociative[A]: Associative[List[A]] =
    Associative.make[List[A]]((l: List[A], r: List[A]) => l ++ r)

  implicit def VectorAssociative[A]: Associative[Vector[A]] =
    Associative.make[Vector[A]]((l: Vector[A], r: Vector[A]) =>  l ++ r)

  implicit def MapAssociative[K, V: Associative]: Associative[Map[K, V]] =
    new Associative[Map[K, V]] {

      def combine(l: Map[K, V], r: Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  implicit def SetAssociative[A]: Associative[Set[A]] =
    Associative.make[Set[A]]((l: Set[A], r: Set[A]) => l | r)

  implicit def Tuple2Associative[A: Associative, B: Associative]: Associative[(A, B)] =
    new Associative[(A, B)] {
      def combine(l: (A, B), r: (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  implicit def Tuple3Associative[A: Associative, B: Associative, C: Associative]: Associative[(A, B, C)] =
    new Associative[(A, B, C)] {
      def combine(l: (A, B, C), r: (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  implicit def Tuple4Associative[A: Associative, B: Associative, C: Associative, D: Associative]: Associative[(A, B, C, D)] =
    new Associative[(A, B, C, D)] {
      def combine(l: (A, B, C, D), r: (A, B, C, D)): (A, B, C, D) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
    }

  implicit def Tuple5Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative]
  : Associative[(A, B, C, D, E)] =
    new Associative[(A, B, C, D, E)] {

      def combine(l: (A, B, C, D, E), r: (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5)
    }

  implicit def Tuple6Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative, F: Associative]
  : Associative[(A, B, C, D, E, F)] =
    new Associative[(A, B, C, D, E, F)] {
      def combine(l: (A, B, C, D, E, F), r: (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6)
    }

  implicit def Tuple7Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative, F: Associative, G: Associative]
  : Associative[(A, B, C, D, E, F, G)] =
    new Associative[(A, B, C, D, E, F, G)] {
      def combine(l: (A, B, C, D, E, F, G), r: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7)
    }

  implicit def Tuple8Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative
  ]: Associative[(A, B, C, D, E, F, G, H)] =
    new Associative[(A, B, C, D, E, F, G, H)] {

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

  implicit def Tuple9Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I)] =
    new Associative[(A, B, C, D, E, F, G, H, I)] {

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

  implicit def Tuple10Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J)] {

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

  implicit def Tuple11Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K)] {

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

  implicit def Tuple12Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L)] {

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

  implicit def Tuple13Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {

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

  implicit def Tuple14Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {

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

  implicit def Tuple15Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {

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

  implicit def Tuple16Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {

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

  implicit def Tuple17Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {

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

  implicit def Tuple18Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {

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

  implicit def Tuple19Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {

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

  implicit def Tuple20Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {

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

  implicit def Tuple21Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative,
    U: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {

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

  implicit def Tuple22Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative,
    U: Associative,
    V: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {

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


