package zio.prelude

import scala.annotation.{ implicitNotFound, tailrec }

import zio.Chunk
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Ord[A]` provides implicit evidence that values of type `A` have a total
 * ordering.
 */
@implicitNotFound("No implicit Ord defined for ${A}.")
trait Ord[-A] extends Equal[A] { self =>

  /**
   * Returns the result of comparing two values of type `A`.
   */
  final def compare(l: A, r: A): Ordering =
    if (Equal.refEq(l, r)) Ordering.Equals else checkCompare(l, r)

  /**
   * Returns the result of comparing two values of type `A`.
   */
  protected def checkCompare(l: A, r: A): Ordering

  override protected def checkEqual(l: A, r: A): Boolean =
    compare(l, r).isEqual

  /**
   * Constructs an `Ord[(A, B)]` given an `Ord[A]` and `Ord[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  final def both[B](that: => Ord[B]): Ord[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  final def bothWith[B, C](that: => Ord[B])(f: C => (A, B)): Ord[C] =
    Ord.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.compare(a1, a2) <> that.compare(b1, b2)
      }
    }

  /**
   * Constructs an `Ord[B]` given an `Ord[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  override def contramap[B](f: B => A): Ord[B] =
    Ord.make((b1, b2) => compare(f(b1), f(b2)))

  /**
   * Constructs an `Ord[Either[A, B]]` given an `Ord[A]` and an `Ord[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  final def either[B](that: => Ord[B]): Ord[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Ord[C]` given an `Ord[A]`, an `Ord[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  final def eitherWith[B, C](that: => Ord[B])(f: C => Either[A, B]): Ord[C] =
    Ord.make { (c1, c2) =>
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
  final def mapOrdering(f: Ordering => Ordering): Ord[A] =
    Ord.make((l, r) => f(compare(l, r)))

  /**
   * Returns a new ordering that is the reverse of this one.
   */
  final def reverse: Ord[A] =
    mapOrdering(_.opposite)

  final def toScalaOrdering[A1 <: A]: scala.math.Ordering[A1] =
    new scala.math.Ordering[A1] {
      def compare(l: A1, r: A1): Int =
        self.compare(l, r) match {
          case Ordering.LessThan    => -1
          case Ordering.Equals      => 0
          case Ordering.GreaterThan => 0
        }
    }
}

object Ord extends Lawful[Ord] {

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is less than `a2` and `a2` is
   * less than `a3` then `a1` is less than `a3`.
   */
  val transitivityLaw1: Laws.Law3[Ord] =
    new Laws.Law3[Ord]("transitivityLaw1") {
      def apply[A: Ord](a1: A, a2: A, a3: A): TestResult =
        ((a1 less a2) && (a2 less a3)) ==> (a1 less a3)
    }

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is greater than `a2` and `a2`
   * is greater than `a3` then `a1` is greater than `a3`.
   */
  val transitivityLaw2: Laws.Law3[Ord] =
    new Laws.Law3[Ord]("transitivityLaw2") {
      def apply[A: Ord](a1: A, a2: A, a3: A): TestResult =
        ((a1 greater a2) && (a2 greater a3)) ==> (a1 greater a3)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is less than or equal to `a2` and
   * `a2` is less than or equal to `a1` then `a1` is equal to `a2`.
   */
  val antisymmetryLaw1: Laws.Law2[Ord] =
    new Laws.Law2[Ord]("antisymmetryLaw1") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        ((a1 lessOrEqual a2) && (a2 lessOrEqual a1)) ==> (a1 equal a2)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is greater than or equal to `a2` and
   * `a2` is greater than or equal to `a1` then `a1` is equal to `a2`.
   */
  val antisymmetryLaw2: Laws.Law2[Ord] =
    new Laws.Law2[Ord]("antisymmetryLaw2") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        ((a1 greaterOrEqual a2) && (a2 greaterOrEqual a1)) ==> (a1 equal a2)
    }

  /**
   * For all values `a1` and `a2`, `a1` is less than or equal to `a2` or `a2`
   * is less than or equal to `a1`.
   */
  val connexityLaw1: Laws.Law2[Ord] =
    new Laws.Law2[Ord]("connexityLaw1") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 lessOrEqual a2) || (a2 lessOrEqual a1)
    }

  /**
   * For all values `a1` and `a2`, `a1` is greater than or equal to `a2` or
   * `a2` is greater than or equal to `a1`.
   */
  val connexityLaw2: Laws.Law2[Ord] =
    new Laws.Law2[Ord]("connexityLaw2") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 greaterOrEqual a2) || (a2 greaterOrEqual a1)
    }

  /**
   * For all values `a1` and `a2`, `a1` is less than or equal to `a2` if and
   * only if `a2` is greater than or equal to `a1`.
   */
  val complementLaw: Laws.Law2[Ord] =
    new Laws.Law2[Ord]("complementLaw") {
      def apply[A: Ord](a1: A, a2: A): TestResult =
        (a1 lessOrEqual a2) <==> (a2 greaterOrEqual a1)
    }

  /**
   * The set of all laws that instances of `Ord` must satify.
   */
  val laws: Laws[Ord] =
    transitivityLaw1 +
      transitivityLaw2 +
      antisymmetryLaw1 +
      antisymmetryLaw2 +
      connexityLaw1 +
      connexityLaw2 +
      complementLaw +
      Equal.laws

  /**
   * The contravariant instance for `Ord`.
   */
  implicit val OrdContravariant: Contravariant[Ord] =
    new Contravariant[Ord] {
      def contramap[A, B](f: B => A): Ord[A] => Ord[B] =
        _.contramap(f)
    }

  /**
   * Summons an implicit `Ord[A]`.
   */
  def apply[A](implicit ord: Ord[A]): Ord[A] =
    ord

  /**
   * Constructs an `Ord[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def make[A](ord: (A, A) => Ordering): Ord[A] =
    (l, r) => if (Equal.refEq(l, r)) Ordering.Equals else ord(l, r)

  /**
   * Constructs an `Ord[A]` from a [[scala.math.Ordering]].
   */
  def default[A](implicit ord: scala.math.Ordering[A]): Ord[A] =
    make((a1, a2) => Ordering.fromCompare(ord.compare(a1, a2)))

  /**
   * Ordering for `Boolean` values.
   */
  implicit val BoolOrd: Ord[Boolean] =
    default

  /**
   * Ordering for `Byte` values.
   */
  implicit val ByteOrd: Ord[Byte] =
    default

  /**
   * Ordering for `Char` values.
   */
  implicit val CharOrd: Ord[Char] =
    default

  /**
   * Derives an `Ord[Vector[A]]` given an `Ord[A]`.
   */
  implicit def ChunkOrd[A: Ord]: Ord[Chunk[A]] =
    make { (l, r) =>
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

  /**
   * Ordering for `Double` values. Note that to honor the contract of a total
   * ordering, `Double.NaN` will be treated as greater than any other number.
   */
  implicit val DoubleOrd: Ord[Double] =
    make((n1, n2) => Ordering.fromCompare(java.lang.Double.compare(n1, n2)))

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
    make((n1, n2) => Ordering.fromCompare(java.lang.Float.compare(n1, n2)))

  /**
   * Ordering for `Int` values.
   */
  implicit val IntOrd: Ord[Int] =
    default

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

    make((l, r) => loop(l, r))
  }

  /**
   * Ordering for `Long` values.
   */
  implicit val LongOrd: Ord[Long] =
    default

  /**
   * Ordering for `Nothing` values. Note that since there are not values of
   * type `Nothing` the `ord` method of this instance can never be called
   * but it can be useful in deriving instances for more complex types.
   */
  implicit val NothingOrd: Ord[Nothing] =
    make[Nothing]((l: Nothing, _: Nothing) => l)

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
    default

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple2Ord[A: Ord, B: Ord]: Ord[(A, B)] =
    Ord[A] both Ord[B]

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple3Ord[A: Ord, B: Ord, C: Ord]: Ord[(A, B, C)] =
    make {
      case ((a1, b1, c1), (a2, b2, c2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple4Ord[A: Ord, B: Ord, C: Ord, D: Ord]: Ord[(A, B, C, D)] =
    make {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple5Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord]: Ord[(A, B, C, D, E)] =
    make {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple6Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord]: Ord[(A, B, C, D, E, F)] =
    make {
      case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple7Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord]: Ord[(A, B, C, D, E, F, G)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple8Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord]
    : Ord[(A, B, C, D, E, F, G, H)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2)
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple9Ord[A: Ord, B: Ord, C: Ord, D: Ord, E: Ord, F: Ord, G: Ord, H: Ord, I: Ord]
    : Ord[(A, B, C, D, E, F, G, H, I)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2)
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
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2)
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
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2)
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
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2)
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
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
          ) =>
        (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2) <> (v1 =?= v2)
    }

  /**
   * Equality for `Unit` values. Since there is only one `Unit` value all
   * values will be equal.
   */
  implicit val UnitOrd: Ord[Unit] =
    make((_, _) => Ordering.Equals)

  /**
   * Derives an `Ord[Vector[A]]` given an `Ord[A]`.
   */
  implicit def VectorOrd[A: Ord]: Ord[Vector[A]] =
    make { (l, r) =>
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
  implicit class OrdOps[A](val l: A) {

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
    case Ordering.GreaterThan => true
    case _                    => false
  }

  /**
   * Returns whether this `Ordering` is `Ordering.LessThan`.
   */
  final def isLessThan: Boolean = self match {
    case Ordering.LessThan => true
    case _                 => false
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
