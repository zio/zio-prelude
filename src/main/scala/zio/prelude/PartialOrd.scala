package zio.prelude

import scala.annotation.{ implicitNotFound, tailrec }

import zio.prelude.Equal._
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }
import zio.{ Chunk, NonEmptyChunk }

/**
 * `PartialOrd[A]` provides implicit evidence that values of type `A` have a partial
 * ordering.
 */
@implicitNotFound("No implicit PartialOrd defined for ${A}.")
trait PartialOrd[-A] extends Equal[A] { self =>

  /**
   * Returns the result of comparing two values of type `A`.
   */
  def compare(l: A, r: A): PartialOrdering =
    if (Equal.refEq(l, r)) Ordering.Equals else checkCompare(l, r)

  /**
   * Returns the result of comparing two values of type `A`.
   */
  protected def checkCompare(l: A, r: A): PartialOrdering

  override protected def checkEqual(l: A, r: A): Boolean =
    compare(l, r).isEqual

  /**
   * Constructs an `PartialOrd[(A, B)]` given an `PartialOrd[A]` and `PartialOrd[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  final def both[B](that: => PartialOrd[B]): PartialOrd[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `PartialOrd[C]` given an `PartialOrd[A]`, an `PartialOrd[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  final def bothWith[B, C](that: => PartialOrd[B])(f: C => (A, B)): PartialOrd[C] =
    PartialOrd.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.compare(a1, a2) <> that.compare(b1, b2)
      }
    }

  /**
   * Constructs an `PartialOrd[B]` given an `PartialOrd[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  override def contramap[B](f: B => A): PartialOrd[B] =
    PartialOrd.make((b1, b2) => compare(f(b1), f(b2)))

  /**
   * Constructs an `PartialOrd[Either[A, B]]` given an `PartialOrd[A]` and an `PartialOrd[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  final def either[B](that: => PartialOrd[B]): PartialOrd[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `PartialOrd[C]` given an `PartialOrd[A]`, an `PartialOrd[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  final def eitherWith[B, C](that: => PartialOrd[B])(f: C => Either[A, B]): PartialOrd[C] =
    PartialOrd.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.compare(a1, a2)
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => that.compare(b1, b2)
      }
    }

  /**
   * Constructs a new `PartialOrd[A]` by mapping the result of this ordering using the
   * specified function.
   */
  final def mapPartialOrdering(f: PartialOrdering => PartialOrdering): PartialOrd[A] =
    PartialOrd.make((l, r) => f(compare(l, r)))

}

object PartialOrd extends Lawful[PartialOrd] {

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is less than `a2` and `a2` is
   * less than `a3` then `a1` is less than `a3`.
   */
  val transitivityLaw1: Laws[PartialOrd] =
    new Laws.Law3[PartialOrd]("transitivityLaw1") {
      def apply[A: PartialOrd](a1: A, a2: A, a3: A): TestResult =
        ((a1 less a2) && (a2 less a3)) ==> (a1 less a3)
    }

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is greater than `a2` and `a2`
   * is greater than `a3` then `a1` is greater than `a3`.
   */
  val transitivityLaw2: Laws[PartialOrd] =
    new Laws.Law3[PartialOrd]("transitivityLaw2") {
      def apply[A: PartialOrd](a1: A, a2: A, a3: A): TestResult =
        ((a1 greater a2) && (a2 greater a3)) ==> (a1 greater a3)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is less than or equal to `a2` and
   * `a2` is less than or equal to `a1` then `a1` is equal to `a2`.
   */
  val antisymmetryLaw1: Laws[PartialOrd] =
    new Laws.Law2[PartialOrd]("antisymmetryLaw1") {
      def apply[A: PartialOrd](a1: A, a2: A): TestResult =
        ((a1 lessOrEqual a2) && (a2 lessOrEqual a1)) ==> (a1 isEqualTo a2)
    }

  /**
   * For all values `a1` and `a2`, if `a1` is greater than or equal to `a2` and
   * `a2` is greater than or equal to `a1` then `a1` is equal to `a2`.
   */
  val antisymmetryLaw2: Laws[PartialOrd] =
    new Laws.Law2[PartialOrd]("antisymmetryLaw2") {
      def apply[A: PartialOrd](a1: A, a2: A): TestResult =
        ((a1 greaterOrEqual a2) && (a2 greaterOrEqual a1)) ==> (a1 isEqualTo a2)
    }

  /**
   * The set of all laws that instances of `PartialOrd` must satisfy.
   */
  val laws: Laws[PartialOrd] =
    transitivityLaw1 +
      transitivityLaw2 +
      antisymmetryLaw1 +
      antisymmetryLaw2 +
      Equal.laws

  /**
   * The `AssociativeBoth` instance for `PartialOrd`.
   */
  implicit val PartialOrdAssociativeBoth: AssociativeBoth[PartialOrd] =
    new AssociativeBoth[PartialOrd] {
      def both[A, B](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `AssociativeEither` instance for `PartialOrd`.
   */
  implicit val PartialOrdAssociativeEither: AssociativeEither[PartialOrd] =
    new AssociativeEither[PartialOrd] {
      def either[A, B](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[Either[A, B]] =
        fa.either(fb)
    }

  /**
   * The `Contravariant` instance for `PartialOrd`.
   */
  implicit val PartialOrdContravariant: Contravariant[PartialOrd] =
    new Contravariant[PartialOrd] {
      def contramap[A, B](f: B => A): PartialOrd[A] => PartialOrd[B] =
        _.contramap(f)
    }

  /**
   * The `IdentityBoth` instance for `PartialOrd`.
   */
  implicit val PartialOrdIdentityBoth: IdentityBoth[PartialOrd] =
    new IdentityBoth[PartialOrd] {
      val any: PartialOrd[Any]                                                       =
        AnyHashOrd
      def both[A, B](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `IdentityEither` instance for `PartialOrd`.
   */
  implicit val PartialOrdIdentityEither: IdentityEither[PartialOrd] =
    new IdentityEither[PartialOrd] {
      def either[A, B](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[Either[A, B]] =
        fa.either(fb)
      val none: PartialOrd[Nothing]                                                          =
        NothingHashOrd
    }

  /**
   * Summons an implicit `PartialOrd[A]`.
   */
  def apply[A](implicit ord: PartialOrd[A]): PartialOrd[A] =
    ord

  /**
   * Constructs an `PartialOrd[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def make[A](ord: (A, A) => PartialOrdering): PartialOrd[A] =
    (l, r) => ord(l, r)

  /**
   * Constructs an instance from an `ord` function and a `equal0` function.
   * Since this takes a separate `equal0`, short-circuiting the equality check (failing fast) is possible.
   */
  def makeFrom[A](ord: (A, A) => PartialOrdering, equal0: Equal[A]): PartialOrd[A] =
    new PartialOrd[A] {
      override protected def checkCompare(l: A, r: A): PartialOrdering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean           = equal0.equal(l, r)
    }

  /**
   * Derives an `PartialOrd[Chunk[A]]` given an `PartialOrd[A]`.
   */
  implicit def ChunkPartialOrd[A: PartialOrd]: PartialOrd[Chunk[A]] =
    makeFrom(
      { (l, r) =>
        val j = l.length
        val k = r.length

        @tailrec
        def loop(i: Int): PartialOrdering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else {
            val compare = PartialOrd[A].compare(l(i), r(i))
            if (compare.isEqual) loop(i + 1) else compare
          }

        loop(0)
      },
      Equal.ChunkEqual
    )

  /**
   * Derives an `PartialOrd[F[A]]` given a `Derive[F, PartialOrd]` and an `PartialOrd[A]`.
   */
  implicit def DerivePartialOrd[F[_], A](implicit derive: Derive[F, PartialOrd], ord: PartialOrd[A]): PartialOrd[F[A]] =
    derive.derive(ord)

  /**
   * Derives an `PartialOrd[Either[A, B]]` given an `PartialOrd[A]` and an `PartialOrd[B]`.
   */
  implicit def EitherPartialOrd[A: PartialOrd, B: PartialOrd]: PartialOrd[Either[A, B]] =
    makeFrom(
      {
        case (Left(a1), Left(a2))   => a1 =??= a2
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => b1 =??= b2
      },
      Equal.EitherEqual
    )

  /**
   * Derives an `PartialOrd[List[A]]` given an `PartialOrd[A]`.
   */
  implicit def ListPartialOrd[A: PartialOrd]: PartialOrd[List[A]] = {

    @tailrec
    def loop(left: List[A], right: List[A]): PartialOrdering =
      (left, right) match {
        case (Nil, Nil)               => Ordering.Equals
        case (Nil, _)                 => Ordering.LessThan
        case (_, Nil)                 => Ordering.GreaterThan
        case ((h1 :: t1), (h2 :: t2)) =>
          val compare = PartialOrd[A].compare(h1, h2)
          if (compare.isEqual) loop(t1, t2) else compare
      }

    makeFrom((l, r) => loop(l, r), Equal.ListEqual)
  }

  /**
   * Derives an `PartialOrd[Map[A, B]]` given an `PartialOrd[B]`.
   * Due to the limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def MapPartialOrd[A, B: PartialOrd]: PartialOrd[Map[A, B]] =
    PartialOrd.make { (l, r) =>
      def compareValues(lesserMap: Map[A, B], expected: Ordering): PartialOrdering =
        lesserMap.keys.map(k => l(k) =??= r(k)).fold(expected)(_.reduce(_))
      if (l.keySet == r.keySet) {
        compareValues(l, Ordering.Equals)
      } else if (l.keySet.subsetOf(r.keySet)) {
        compareValues(l, Ordering.LessThan)
      } else if (r.keySet.subsetOf(l.keySet)) {
        compareValues(r, Ordering.GreaterThan)
      } else {
        PartialOrdering.NoOrder
      }
    }

  /**
   * Derives an `PartialOrd[NonEmptyChunk[A]]` given an `PartialOrd[A]`.
   */
  implicit def NonEmptyChunkPartialOrd[A: PartialOrd]: PartialOrd[NonEmptyChunk[A]] =
    PartialOrd[Chunk[A]].contramap(_.toChunk)

  /**
   * Derives an `PartialOrd[Option[A]]` given an `PartialOrd[A]`. `None` will be treated as
   * less than all other values.
   */
  implicit def OptionPartialOrd[A: PartialOrd]: PartialOrd[Option[A]] =
    PartialOrd[Unit].eitherWith(PartialOrd[A]) {
      case None    => Left(())
      case Some(a) => Right(a)
    }

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple2PartialOrd[A: PartialOrd, B: PartialOrd]: PartialOrd[(A, B)] =
    makeFrom(
      { case ((a1, b1), (a2, b2)) =>
        (a1 =??= a2) <> (b1 =??= b2)
      },
      Equal.Tuple2Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple3PartialOrd[A: PartialOrd, B: PartialOrd, C: PartialOrd]: PartialOrd[(A, B, C)] =
    makeFrom(
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2)
      },
      Equal.Tuple3Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple4PartialOrd[A: PartialOrd, B: PartialOrd, C: PartialOrd, D: PartialOrd]: PartialOrd[(A, B, C, D)] =
    makeFrom(
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2)
      },
      Equal.Tuple4Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple5PartialOrd[A: PartialOrd, B: PartialOrd, C: PartialOrd, D: PartialOrd, E: PartialOrd]
    : PartialOrd[(A, B, C, D, E)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2)
      },
      Equal.Tuple5Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple6PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2)
      },
      Equal.Tuple6Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple7PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2)
      },
      Equal.Tuple7Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple8PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2)
      },
      Equal.Tuple8Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple9PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2)
      },
      Equal.Tuple9Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple10PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2)
      },
      Equal.Tuple10Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple11PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2)
      },
      Equal.Tuple11Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple12PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2)
      },
      Equal.Tuple12Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple13PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2)
      },
      Equal.Tuple13Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple14PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2)
      },
      Equal.Tuple14Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple15PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2)
      },
      Equal.Tuple15Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple16PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2)
      },
      Equal.Tuple16Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple17PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2)
      },
      Equal.Tuple17Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple18PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd,
    R: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2)
      },
      Equal.Tuple18Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple19PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd,
    R: PartialOrd,
    S: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2)
      },
      Equal.Tuple19Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple20PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd,
    R: PartialOrd,
    S: PartialOrd,
    T: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2)
      },
      Equal.Tuple20Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple21PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd,
    R: PartialOrd,
    S: PartialOrd,
    T: PartialOrd,
    U: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2) <> (u1 =??= u2)
      },
      Equal.Tuple21Equal
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple22PartialOrd[
    A: PartialOrd,
    B: PartialOrd,
    C: PartialOrd,
    D: PartialOrd,
    E: PartialOrd,
    F: PartialOrd,
    G: PartialOrd,
    H: PartialOrd,
    I: PartialOrd,
    J: PartialOrd,
    K: PartialOrd,
    L: PartialOrd,
    M: PartialOrd,
    N: PartialOrd,
    O: PartialOrd,
    P: PartialOrd,
    Q: PartialOrd,
    R: PartialOrd,
    S: PartialOrd,
    T: PartialOrd,
    U: PartialOrd,
    V: PartialOrd
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2) <> (u1 =??= u2) <> (v1 =??= v2)
      },
      Equal.Tuple22Equal
    )

  /**
   * Derives an `PartialOrd[Vector[A]]` given an `PartialOrd[A]`.
   */
  implicit def VectorPartialOrd[A: PartialOrd]: PartialOrd[Vector[A]] =
    make { (l, r) =>
      val j = l.length
      val k = r.length

      def loop(i: Int): PartialOrdering =
        if (i == j && i == k) Ordering.Equals
        else if (i == j) Ordering.LessThan
        else if (i == k) Ordering.GreaterThan
        else {
          val compare = PartialOrd[A].compare(l(i), r(i))
          if (compare.isEqual) loop(i + 1) else compare
        }

      loop(0)
    }
}

trait PartialOrdSyntax {

  /**
   * Provides infix syntax for comparing two values with a total ordering.
   */
  implicit class PartialOrdOps[A](val l: A) {

    /**
     * Returns whether this value is greater than the specified value.
     */
    def >[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      ord.compare(l, r) === Ordering.GreaterThan

    /**
     * Returns whether this value is greater than or equal to the specified
     * value.
     */
    def >=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      (l > r) || (l === r)

    /**
     * Returns whether this value is less than the specified value.
     */
    def <[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      ord.compare(l, r) === Ordering.LessThan

    /**
     * Returns whether this value is less than or equal to the specified
     * value.
     */
    def <=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      (l < r) || (l === r)

    /**
     * Returns the result of comparing this value with the specified value.
     */
    def =??=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): PartialOrdering = ord.compare(l, r)
  }
}
