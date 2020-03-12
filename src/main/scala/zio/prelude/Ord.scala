package zio.prelude

import scala.annotation.{ implicitNotFound, tailrec }

trait OrdLaws[-A] {
  protected implicit val self: Ord[A]

  def compare(l: A, r: A): Ordering

  final def transitivityLaw1(a1: A, a2: A, a3: A): Boolean =
    ((a1 < a2) && (a2 < a3)) ==> (a1 < a3)

  final def transitivityLaw2(a1: A, a2: A, a3: A): Boolean =
    ((a1 > a2) && (a2 > a3)) ==> (a1 > a3)

  final def antisymmetryLaw1(a1: A, a2: A): Boolean =
    ((a1 <= a2) && (a2 <= a1)) ==> (a1 === a2)

  final def antisymmetryLaw2(a1: A, a2: A): Boolean =
    ((a1 >= a2) && (a2 >= a1)) ==> (a1 === a2)

  final def connexityLaw1(a1: A, a2: A): Boolean =
    (a1 <= a2) || (a2 <= a1)

  final def connexityLaw2(a1: A, a2: A): Boolean =
    (a1 >= a2) || (a2 >= a1)

  final def complementLaw(a1: A, a2: A): Boolean =
    (a1 <= a2) <==> (a2 >= a1)
}

/**
 * `Ord[A]` provides implicit evidence that values of type `A` have a total
 * ordering.
 */
@implicitNotFound("No implicit Ord defined for ${A}.")
sealed trait Ord[-A] extends OrdLaws[A] {
  protected implicit val self: Ord[A] = this

  /**
   * Constructs an `Ord[(A, B)]` given an `Ord[A]` and `Ord[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  def both[B](that: Ord[B]): Ord[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  def bothWith[B, C](that: Ord[B])(f: C => (A, B)): Ord[C] =
    Ord { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.compare(a1, a2) <> that.compare(b1, b2)
      }
    }

  /**
   * Constructs an `Ord[B]` given an `Ord[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  def contramap[B](f: B => A): Ord[B] =
    Ord((b1, b2) => self.compare(f(b1), f(b2)))

  /**
   * Constructs an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  def either[B](that: Ord[B]): Ord[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  def eitherWith[B, C](that: Ord[B])(f: C => Either[A, B]): Ord[C] =
    Ord { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.compare(a1, a2)
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => that.compare(b1, b2)
      }
    }

  /**
   * Constructs a new `Ord[A]` by mapping the result of this ordering using the
   * specified function.
   */
  def mapOrdering(f: Ordering => Ordering): Ord[A] =
    Ord((l, r) => f(self.compare(l, r)))

  /**
   * Returns a new ordering that is the reverse of this one.
   */
  def reverse: Ord[A] =
    mapOrdering(_.opposite)
}

object Ord {

  /**
   * Summons an implicit `Ord[A]`.
   */
  def apply[A](implicit ord: Ord[A]): Ord[A] = ord

  /**
   * Constructs an `Ord[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def apply[A](f: (A, A) => Ordering): Ord[A] =
    new Ord[A] {
      def compare(l: A, r: A): Ordering =
        if (Equal.refEq(l, r)) Ordering.Equals else f(l, r)
    }

  /**
   * Constructs an `Ord[A]` from a [[`scala.math.Ordering]].
   */
  def fromScalaOrdering[A](implicit ord: scala.math.Ordering[A]): Ord[A] =
    Ord((a1, a2) => Ordering.fromCompare(ord.compare(a1, a2)))

  /**
   * Ordering for `Boolean` values.
   */
  implicit val BoolOrd: Ord[Boolean] =
    fromScalaOrdering[Boolean]

  /**
   * Ordering for `Byte` values.
   */
  implicit val ByteOrd: Ord[Byte] =
    fromScalaOrdering[Byte]

  /**
   * Ordering for `Char` values.
   */
  implicit val CharOrd: Ord[Char] =
    fromScalaOrdering[Char]

  /**
   * Ordering for `Double` values. Note that to honor the contract of a total
   * ordering, `Double.NaN` will be treated as greater than any other number.
   */
  implicit val DoubleOrd: Ord[Double] =
    Ord((n1, n2) => Ordering.fromCompare(java.lang.Double.compare(n1, n2)))

  /**
   * Derives an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`.
   */
  implicit def EitherOrd[A: Ord, B: Ord]: Ord[Either[A, B]] =
    Ord[A] either Ord[B]

  /**
   * Ordering for `Float` values. Note that to honor the contract of a total
   * ordering, `Flat.NaN` will be treated as greater than any other number.
   */
  implicit val FloatOrd: Ord[Float] =
    Ord((n1, n2) => Ordering.fromCompare(java.lang.Float.compare(n1, n2)))

  /**
   * Ordering for `Int` values.
   */
  implicit val IntOrd: Ord[Int] =
    fromScalaOrdering[Int]

  /**
   * Derives an `Ord[List[A]]` given an `Ord[A]`.
   */
  implicit def ListOrd[A: Ord]: Ord[List[A]] = {

    @tailrec
    def loop[A: Ord](left: List[A], right: List[A]): Ordering =
      (left, right) match {
        case (Nil, Nil) => Ordering.Equals
        case (Nil, _)   => Ordering.LessThan
        case (_, Nil)   => Ordering.GreaterThan
        case ((h1 :: t1), (h2 :: t2)) =>
          val compare = Ord[A].compare(h1, h2)
          if (compare.isEqual) loop(t1, t2) else compare
      }

    Ord((l, r) => loop(l, r))
  }

  /**
   * Ordering for `Long` values.
   */
  implicit val LongOrd: Ord[Long] =
    fromScalaOrdering[Long]

  /**
   * Ordering for `Nothing` values. Note that since there are not values of
   * type `Nothing` the `ord` method of this instance can never be called
   * but it can be useful in deriving instances for more complex types.
   */
  implicit val NothingOrd: Ord[Nothing] =
    Ord[Nothing]((l: Nothing, _: Nothing) => l)

  /**
   * Derives an `Ord[Option[A]]` given an `Ord[A]`. `None` will be treated as
   * less than all other values.
   */
  implicit def OptionOrd[A: Ord]: Ord[Option[A]] =
    Ord[Unit].eitherWith(Ord[A]) {
      case None    => Left(())
      case Some(a) => Right(a)
    }

  /**
   * Ordering for `String` values.
   */
  implicit val StringOrd: Ord[String] =
    fromScalaOrdering[String]

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple2Ord[A: Ord, B: Ord]: Ord[(A, B)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1), (a2, b2)) => (a1 =?= a2) <> (b1 =?= b2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple3Ord[A: Ord, B: Ord, C: Ord]: Ord[(A, B, C)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1), (a2, b2, c2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple4Ord[A: Ord, B: Ord, C: Ord, D: Ord]: Ord[(A, B, C, D)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple5Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord]: Ord[(A, B, C, D, E)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple6Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord]: Ord[(A, B, C, D, E, F)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple7Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord]: Ord[(A, B, C, D, E, F, G)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple8Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord]
    : Ord[(A, B, C, D, E, F, G, H)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple9Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord, I: Ord]
    : Ord[(A, B, C, D, E, F, G, H, I)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple10Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple11Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple12Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    Ord { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple13Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple14Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple15Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple16Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple17Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple18Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple19Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple20Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple21Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord,
    U: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple22Ord[
    A: Ord,
    B: Ord,
    C: Ord,
    D: Ord,
    E: Ord,
    F: Ord,
    G: Ord,
    H: Ord,
    I: Ord,
    J: Ord,
    K: Ord,
    L: Ord,
    M: Ord,
    N: Ord,
    O: Ord,
    P: Ord,
    Q: Ord,
    R: Ord,
    S: Ord,
    T: Ord,
    U: Ord,
    V: Ord
  ]: Ord[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    Ord { (a, b) =>
      (a, b) match {
        case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2) <> (v1 =?= v2)
      }
    }

  /**
   * Equality for `Unit` values. Since there is only one `Unit` value all
   * values will be equal.
   */
  implicit val UnitOrd: Ord[Unit] =
    Ord((_, _) => Ordering.Equals)

  /**
   * Derives an `Ord[Vector[A]]` given an `Ord[A]`.
   */
  implicit def VectorOrd[A: Ord]: Ord[Vector[A]] =
    Ord { (l, r) =>
      val j = l.length
      val k = r.length

      def loop(i: Int): Ordering =
        if (i == j && i == k) Ordering.Equals
        else if (i == j) Ordering.LessThan
        else if (i == k) Ordering.GreaterThan
        else {
          val compare = Ord[A].compare(l(i), r(i))
          if (compare.isEqual) loop(i + 1) else compare
        }

      loop(0)
    }
}

trait OrdSyntax {

  /**
   * Provides infix syntax for comparing two values with a total ordering.
   */
  implicit class OrdSyntax[A](val l: A) {

    /**
     * Returns whether this value is greater than the specified value.
     */
    def >(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.GreaterThan

    /**
     * Returns whether this value is greater than or equal to the specified
     * value.
     */
    def >=(r: A)(implicit ord: Ord[A]): Boolean =
      (l > r) || (l === r)

    /**
     * Returns whether this value is less than the specified value.
     */
    def <(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.LessThan

    /**
     * Returns whether this value is less than or equal to the specified
     * value.
     */
    def <=(r: A)(implicit ord: Ord[A]): Boolean =
      (l < r) || (l === r)

    /**
     * Returns the result of comparing this value with the specified value.
     */
    def =?=(r: A)(implicit ord: Ord[A]): Ordering = ord.compare(l, r)
  }
}

/**
 * An `Ordering` is the result of comparing two values. The result may be
 * `LessThan`, `Equals`, or `GreaterThan`.
 */
sealed trait Ordering { self =>

  /**
   * A symbolic alias for `orElse`.
   */
  final def <>(that: => Ordering): Ordering =
    self orElse that

  /**
   * Returns whether this `Ordering` is `Ordering.Equals`.
   */
  final def isEqual: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Returns whether this `Ordering` is `Ordering.GreaterThan`.
   */
  final def isGreaterThan: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Returns whether this `Ordering` is `Ordering.LessThan`.
   */
  final def isLessThan: Boolean = self match {
    case Ordering.Equals => true
    case _               => false
  }

  /**
   * Converts this `Ordering` to an ordinal representation, with `0`
   * representing `LessThan`, `1` representing `Equals` and `2` representing
   * `GreaterThan`.
   */
  final def ordinal: Int = self match {
    case Ordering.LessThan    => 0
    case Ordering.Equals      => 1
    case Ordering.GreaterThan => 2
  }

  /**
   * Returns this ordering, but if this ordering is equal returns the
   * specified ordering.
   */
  final def orElse(that: => Ordering): Ordering = self match {
    case Ordering.Equals => that
    case ordering        => ordering
  }

  /**
   * Returns the opposite of this `Ordering`, with `LessThan` converted to
   * `GreaterThan` and `GreaterThan` converted to `LessThan`.
   */
  final def opposite: Ordering = self match {
    case Ordering.LessThan    => Ordering.GreaterThan
    case Ordering.Equals      => Ordering.Equals
    case Ordering.GreaterThan => Ordering.LessThan
  }
}

object Ordering {
  case object LessThan    extends Ordering
  case object Equals      extends Ordering
  case object GreaterThan extends Ordering

  /**
   * Converts an integer result from [[scala.math.Ordering.compare]] or
   * [[java.lang.Comparable]] to a `Compare`.
   */
  def fromCompare(n: Int): Ordering =
    if (n < 0) LessThan
    else if (n > 0) GreaterThan
    else Equals

  /**
   * Ordering for `Ordering` values.
   */
  implicit val orderingOrdering: Ord[Ordering] =
    Ord((l: Ordering, r: Ordering) => Ord[Int].compare(l.ordinal, r.ordinal))
}
