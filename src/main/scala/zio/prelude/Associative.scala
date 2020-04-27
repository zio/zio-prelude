package zio.prelude

import zio.{ Chunk, NonEmptyChunk }
import zio.prelude.coherent.AssociativeEqual
import zio.prelude.newtypes.{ And, First, Last, Max, Min, Or, Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Associative[A]` type class describes an associative binary operator
 * for a type `A`. For example, addition for integers, and string
 * concatenation for strings.
 */
trait Associative[A] extends Closure[A]

object Associative extends Lawful[AssociativeEqual] {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  final val associativityLaw = new Laws.Law3[AssociativeEqual]("associativityLaw") {
    def apply[A: AssociativeEqual](a1: A, a2: A, a3: A): TestResult =
      (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
  }

  final val laws = associativityLaw + Closure.laws

  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  def make[A](f: (A, A) => A): Associative[A] =
    (l, r) => f(l, r)

  implicit val BooleanConjunctionAssociative: Associative[And] =
    make[And]((l, r) => And(l && r))

  implicit val BooleanDisjunctionAssociative: Associative[Or] =
    make[Or]((l, r) => Or(l || r))

  implicit val ByteProdAssociative: Associative[Prod[Byte]] =
    make[Prod[Byte]]((l, r) => Prod((l * r).toByte))

  implicit val ByteSumAssociative: Associative[Sum[Byte]] =
    make[Sum[Byte]]((l, r) => Sum((l + r).toByte))

  implicit val CharProdAssociative: Associative[Prod[Char]] =
    make((l, r) => Prod((l * r).toChar))

  implicit val CharSumIdentity: Associative[Sum[Char]] =
    make((l, r) => Sum((l + r).toChar))

  implicit def ChunkAssociative[A]: Associative[Chunk[A]] =
    make(_ ++ _)

  implicit val DoubleProdAssociative: Associative[Prod[Double]] =
    make[Prod[Double]]((l, r) => Prod(l * r))

  implicit val DoubleSumAssociative: Associative[Sum[Double]] =
    make[Sum[Double]]((l, r) => Sum(l + r))

  implicit def firstAssociative[A]: Associative[First[A]] =
    make((l: First[A], _: First[A]) => l)

  implicit val FloatProdAssociative: Associative[Prod[Float]] =
    make[Prod[Float]]((l, r) => Prod(l * r))

  implicit val FloatSumAssociative: Associative[Sum[Float]] =
    make[Sum[Float]]((l, r) => Sum(l + r))

  implicit val IntProdAssociative: Associative[Prod[Int]] =
    make[Prod[Int]]((l, r) => Prod(l * r))

  implicit val IntSumAssociative: Associative[Sum[Int]] =
    make[Sum[Int]]((l, r) => Sum(l + r))

  implicit def lastAssociative[A]: Associative[Last[A]] =
    make((_: Last[A], r: Last[A]) => r)

  implicit def ListAssociative[A]: Associative[List[A]] =
    make[List[A]]((l, r) => l ++ r)

  implicit val LongProdAssociative: Associative[Prod[Long]] =
    make[Prod[Long]]((l, r) => Prod(l * r))

  implicit val LongSumAssociative: Associative[Sum[Long]] =
    make[Sum[Long]]((l, r) => Sum(l + r))

  implicit def MapAssociative[K, V: Associative]: Associative[Map[K, V]] =
    make[Map[K, V]]((l, r) =>
      r.foldLeft(l) {
        case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
      }
    )

  implicit def maxAssociative[A: Ord]: Associative[Max[A]] =
    make((l: Max[A], r: Max[A]) => if (l >= r) l else r)

  implicit def minAssociative[A: Ord]: Associative[Min[A]] =
    make((l: Min[A], r: Min[A]) => if (l <= r) l else r)

  implicit def NonEmptyChunkAssociative[A]: Associative[NonEmptyChunk[A]] =
    make(_ ++ _)

  implicit def OptionAssociative[A: Associative]: Associative[Option[A]] =
    make[Option[A]]((l, r) =>
      (l, r) match {
        case (Some(l), Some(r)) => Some(l <> r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case _                  => None
      }
    )

  implicit def SetAssociative[A]: Associative[Set[A]] =
    make[Set[A]]((l, r) => l | r)

  implicit val ShortProdAssociative: Associative[Prod[Short]] =
    make[Prod[Short]]((l, r) => Prod((l * r).toShort))

  implicit val ShortSumAssociative: Associative[Sum[Short]] =
    make[Sum[Short]]((l, r) => Sum((l + r).toShort))

  implicit val StringAssociative: Associative[String] =
    make[String]((l, r) => l + r)

  implicit def VectorAssociative[A]: Associative[Vector[A]] =
    make[Vector[A]]((l, r) => l ++ r)

  implicit def Tuple2Associative[A: Associative, B: Associative]: Associative[(A, B)] =
    make {
      case ((a1, b1), (a2, b2)) => (a1 <> a2, b1 <> b2)
    }

  implicit def Tuple3Associative[A: Associative, B: Associative, C: Associative]: Associative[(A, B, C)] =
    make {
      case ((a1, b1, c1), (a2, b2, c2)) => (a1 <> a2, b1 <> b2, c1 <> c2)
    }

  implicit def Tuple4Associative[A: Associative, B: Associative, C: Associative, D: Associative]
    : Associative[(A, B, C, D)] =
    make {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2)
    }

  implicit def Tuple5Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative]
    : Associative[(A, B, C, D, E)] =
    make {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2)
    }

  implicit def Tuple6Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative
  ]: Associative[(A, B, C, D, E, F)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1),
          (a2, b2, c2, d2, e2, f2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2)
    }

  implicit def Tuple7Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative
  ]: Associative[(A, B, C, D, E, F, G)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1),
          (a2, b2, c2, d2, e2, f2, g2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1),
          (a2, b2, c2, d2, e2, f2, g2, h2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2, i1 <> i2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2,
          u1 <> u2
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2,
          u1 <> u2,
          v1 <> v2
        )
    }
}
