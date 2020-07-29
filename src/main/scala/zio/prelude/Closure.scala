package zio.prelude

import zio.{ Chunk, NonEmptyChunk }
import zio.prelude.newtypes.{ And, Max, Min, Or, Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Closure[A] {
  def combine(l: => A, r: => A): A
}

object Closure extends Lawful[Closure] {

  val closureLaw: Laws[Closure] =
    new Laws.Law2[Closure]("closureLaw") {
      def apply[A: Closure](a1: A, a2: A): TestResult =
        (try {
          (a1 <> a2) != null
        } catch {
          case _: Throwable => false
        }) <-> true
    }

  val laws: Laws[Closure] =
    closureLaw

  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def make[A](f: (A, A) => A): Closure[A] =
    (l, r) => f(l, r)

  implicit val BooleanConjunctionClosure: Closure[And] =
    make[And]((l, r) => And(l && r))

  implicit val BooleanDisjunctionClosure: Closure[Or] =
    make[Or]((l, r) => Or(l || r))

  implicit val BooleanProdClosure: Closure[Prod[Boolean]] =
    make[Prod[Boolean]]((l, r) => Prod(l && r))

  implicit val BooleanSumClosure: Closure[Sum[Boolean]] =
    make[Sum[Boolean]]((l, r) => Sum(l || r))

  implicit val ByteProdClosure: Closure[Prod[Byte]] =
    make[Prod[Byte]]((l, r) => Prod((l * r).toByte))

  implicit val ByteSumClosure: Closure[Sum[Byte]] =
    make[Sum[Byte]]((l, r) => Sum((l + r).toByte))

  implicit val CharProdClosure: Closure[Prod[Char]] =
    make((l, r) => Prod((l * r).toChar))

  implicit val CharSumIdentity: Closure[Sum[Char]] =
    make((l, r) => Sum((l + r).toChar))

  implicit def ChunkClosure[A]: Closure[Chunk[A]] =
    make(_ ++ _)

  /**
   * Derives a `Closure[F[A]]` given a `Derive[F, Closure]` and a `Closure[A]`.
   */
  implicit def DeriveClosure[F[_], A](implicit derive: Derive[F, Closure], closure: Closure[A]): Closure[F[A]] =
    derive.derive(closure)

  implicit val DoubleProdClosure: Closure[Prod[Double]] =
    make[Prod[Double]]((l, r) => Prod(l * r))

  implicit val DoubleSumClosure: Closure[Sum[Double]] =
    make[Sum[Double]]((l, r) => Sum(l + r))

  implicit val FloatProdClosure: Closure[Prod[Float]] =
    make[Prod[Float]]((l, r) => Prod(l * r))

  implicit val FloatSumClosure: Closure[Sum[Float]] =
    make[Sum[Float]]((l, r) => Sum(l + r))

  implicit val IntProdClosure: Closure[Prod[Int]] =
    make[Prod[Int]]((l, r) => Prod(l * r))

  implicit val IntSumClosure: Closure[Sum[Int]] =
    make[Sum[Int]]((l, r) => Sum(l + r))

  implicit def ListClosure[A]: Closure[List[A]] =
    make[List[A]]((l, r) => l ++ r)

  implicit val LongProdClosure: Closure[Prod[Long]] =
    make[Prod[Long]]((l, r) => Prod(l * r))

  implicit val LongSumClosure: Closure[Sum[Long]] =
    make[Sum[Long]]((l, r) => Sum(l + r))

  implicit def MapClosure[K, V: Closure]: Closure[Map[K, V]] =
    make[Map[K, V]]((l, r) =>
      r.foldLeft(l) {
        case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
      }
    )

  implicit def MaxClosure[A: Ord]: Closure[Max[A]] =
    make((l: Max[A], r: Max[A]) => if (l >= r) l else r)

  implicit def MinClosure[A: Ord]: Closure[Min[A]] =
    make((l: Min[A], r: Min[A]) => if (l <= r) l else r)

  implicit def NonEmptyChunkClosure[A]: Closure[NonEmptyChunk[A]] =
    make(_ ++ _)

  implicit def OptionClosure[A: Closure]: Closure[Option[A]] =
    make[Option[A]]((l, r) =>
      (l, r) match {
        case (Some(l), Some(r)) => Some(l <> r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case _                  => None
      }
    )

  implicit def SetClosure[A]: Closure[Set[A]] =
    make[Set[A]]((l, r) => l | r)

  implicit val ShortProdClosure: Closure[Prod[Short]] =
    make[Prod[Short]]((l, r) => Prod((l * r).toShort))

  implicit val ShortSumClosure: Closure[Sum[Short]] =
    make[Sum[Short]]((l, r) => Sum((l + r).toShort))

  implicit val StringClosure: Closure[String] =
    make[String]((l, r) => l + r)

  implicit def VectorClosure[A]: Closure[Vector[A]] =
    make[Vector[A]]((l, r) => l ++ r)

  implicit def Tuple2Closure[A: Closure, B: Closure]: Closure[(A, B)] =
    make {
      case ((a1, b1), (a2, b2)) => (a1 <> a2, b1 <> b2)
    }

  implicit def Tuple3Closure[A: Closure, B: Closure, C: Closure]: Closure[(A, B, C)] =
    make {
      case ((a1, b1, c1), (a2, b2, c2)) => (a1 <> a2, b1 <> b2, c1 <> c2)
    }

  implicit def Tuple4Closure[A: Closure, B: Closure, C: Closure, D: Closure]: Closure[(A, B, C, D)] =
    make {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2)
    }

  implicit def Tuple5Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure]: Closure[(A, B, C, D, E)] =
    make {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2)
    }

  implicit def Tuple6Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure, F: Closure]
    : Closure[(A, B, C, D, E, F)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1),
          (a2, b2, c2, d2, e2, f2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2)
    }

  implicit def Tuple7Closure[A: Closure, B: Closure, C: Closure, D: Closure, E: Closure, F: Closure, G: Closure]
    : Closure[(A, B, C, D, E, F, G)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1),
          (a2, b2, c2, d2, e2, f2, g2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1),
          (a2, b2, c2, d2, e2, f2, g2, h2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2, i1 <> i2)
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

trait ClosureSyntax {

  implicit class ClosureOps[A](l: A) {
    def combine(r: => A)(implicit closure: Closure[A]): A = closure.combine(l, r)

    def <>(r: => A)(implicit closure: Closure[A]): A = closure.combine(l, r)
  }

}
