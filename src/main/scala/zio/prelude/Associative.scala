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

  implicit val CharAssociative: Associative[Char] = Identity.CharIdentity

  implicit val StringAssociative: Associative[String] = Identity.StringIdentity

  implicit val ByteSumAssociative: Associative[Sum[Byte]] = Identity.ByteSumIdentity

  implicit val ByteProdAssociative: Associative[Prod[Byte]] = Identity.ByteProdIdentity

  implicit val ShortSumAssociative: Associative[Sum[Short]] = Identity.ShortSumIdentity

  implicit val ShortProdAssociative: Associative[Prod[Short]] = Identity.ShortProdIdentity

  implicit val IntSumAssociative: Associative[Sum[Int]] = Identity.IntSumIdentity

  implicit val IntProdAssociative: Associative[Prod[Int]] = Identity.IntProdIdentity

  implicit val LongSumAssociative: Associative[Sum[Long]] = Identity.LongSumIdentity

  implicit val LongProdAssociative: Associative[Prod[Long]] = Identity.LongProdIdentity

  implicit val FloatSumAssociative: Associative[Sum[Float]] = Identity.FloatSumIdentity

  implicit val FloatProdAssociative: Associative[Prod[Float]] = Identity.FloatProdIdentity

  implicit val DoubleSumAssociative: Associative[Sum[Double]] = Identity.DoubleSumIdentity

  implicit val DoubleProdAssociative: Associative[Prod[Double]] = Identity.DoubleProdIdentity

  implicit val BooleanDisjunctionAssociative: Associative[Or] = Identity.BooleanDisjunctionIdentity

  implicit val BooleanConjunctionAssociative: Associative[And] = Identity.BooleanConjunctionIdentity

  implicit def OptionAssociative[A: Associative]: Associative[Option[A]] = Identity.OptionIdentity[A]

  implicit def EitherAssociative[E, A: Associative]: Associative[Either[E, A]]  =
    new Associative[Either[E, A]] {
      def combine(l: Either[E, A], r: Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Left(l), _)         => Left(l)
          case (_, Left(r))         => Left(r)
          case (Right(l), Right(r)) => Right(l <> r)
        }
    }

  implicit def ListAssociative[A]: Associative[List[A]] = Identity.ListIdentity

  implicit def VectorAssociative[A]: Associative[Vector[A]] = Identity.VectorIdentity

  implicit def MapAssociative[K, V: Associative]: Associative[Map[K, V]] = Identity.MapIdentity

  implicit def SetAssociative[A]: Associative[Set[A]] = Identity.SetIdentity

  implicit def Tuple2Associative[A: Associative, B: Associative]: Associative[(A, B)]  =
    new Associative[(A, B)] {
      def combine(l: (A, B), r: (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  implicit def Tuple3Associative[A: Associative, B: Associative, C: Associative]: Associative[(A, B, C)] =
    new Associative[(A, B, C)] {
      def combine(l: (A, B, C), r: (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  implicit def Tuple4Associative[A: Associative, B: Associative, C: Associative, D: Associative]: Associative[(A, B, C, D)] =  new Associative[(A, B, C, D)] {
    def combine(l: (A, B, C, D), r: (A, B, C, D)): (A, B, C, D) =
      (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
  }


  //  implicit def Tuple5Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative]
//  : Associative[(A, B, C, D, E)] = Identity.Tuple5Identity
//
//  implicit def Tuple6Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative, F: Associative]
//  : Associative[(A, B, C, D, E, F)] = Identity.Tuple6Identity
//
//  implicit def Tuple7Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative, F: Associative, G: Associative]
//  : Associative[(A, B, C, D, E, F, G)] = Identity.Tuple7Identity
//
//  implicit def Tuple8Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H)] = Identity.Tuple8Identity
//
//  implicit def Tuple9Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I)] = Identity.Tuple9Identity
//
//  implicit def Tuple10Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J)] = Identity.Tuple10Identity
//
//  implicit def Tuple11Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K)] = Identity.Tuple11Identity
//
//  implicit def Tuple12Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L)] = Identity.Tuple12Identity
//
//  implicit def Tuple13Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = Identity.Tuple13Identity
//
//  implicit def Tuple14Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = Identity.Tuple14Identity
//
//  implicit def Tuple15Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = Identity.Tuple15Identity
//
//  implicit def Tuple16Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = Identity.Tuple16Identity
//
//  implicit def Tuple17Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = Identity.Tuple17Identity
//
//  implicit def Tuple18Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative,
//    R: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = Identity.Tuple18Identity
//
//  implicit def Tuple19Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative,
//    R: Associative,
//    S: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = Identity.Tuple19Identity
//
//  implicit def Tuple20Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative,
//    R: Associative,
//    S: Associative,
//    T: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = Identity.Tuple20Identity
//
//  implicit def Tuple21Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative,
//    R: Associative,
//    S: Associative,
//    T: Associative,
//    U: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = Identity.Tuple21Identity
//
//  implicit def Tuple22Associative[
//    A: Associative,
//    B: Associative,
//    C: Associative,
//    D: Associative,
//    E: Associative,
//    F: Associative,
//    G: Associative,
//    H: Associative,
//    I: Associative,
//    J: Associative,
//    K: Associative,
//    L: Associative,
//    M: Associative,
//    N: Associative,
//    O: Associative,
//    P: Associative,
//    Q: Associative,
//    R: Associative,
//    S: Associative,
//    T: Associative,
//    U: Associative,
//    V: Associative
//  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = Identity.Tuple22Identity

}
