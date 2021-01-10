package zio.prelude

import zio.prelude.Equal._
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}
import zio.{Chunk, NonEmptyChunk}
import scala.{math => sm}
import sm.Ordering._

import scala.annotation.{implicitNotFound, tailrec}
import zio.Fiber

/**
 * `PartialOrd[A]` provides implicit evidence that values of type `A` have a partial
 * ordering.
 */
@implicitNotFound("No implicit PartialOrd defined for ${A}.")
abstract class PartialOrd[-A](implicit E: Equal[A]) { self =>
  //// Exported members
  final def equal(l: A, r: A): Boolean =
    E.equal(l, r)

  final def notEqual(l: A, r: A): Boolean =
    E.notEqual(l, r)
  ////

  /**
   * Returns the result of comparing two values of type `A`.
   */
  def compare(l: A, r: A): PartialOrdering =
    if (Equal.refEq(l, r)) Ordering.Equals else checkCompare(l, r)

  /**
   * Returns the result of comparing two values of type `A`, if the order is defined between `l` and `r`.
   */
  protected def checkCompare(l: A, r: A): PartialOrdering

  final protected def checkEqual(l: A, r: A): Boolean =
    compare(l, r).isEqual

  /**
   * Constructs an `PartialOrd[(A, B)]` given an `PartialOrd[A]` and `PartialOrd[B]` by first
   * comparing the `A` values, and then if the `A` values are equal comparing
   * the `B` values
   */
  final def both[B: Equal](that: => PartialOrd[B]): PartialOrd[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `PartialOrd[C]` given an `PartialOrd[A]`, an `PartialOrd[B]` and a function `f`
   * to transform a `C` value into an `(A, B)`. The instance will convert each
   * `C` value into an `(A, B)`, compare the `A` values, and then if the `A`
   * values are equal compare the `B` values.
   */
  final def bothWith[B: Equal, C](that: => PartialOrd[B])(f: C => (A, B)): PartialOrd[C] =
    PartialOrd.make[C] { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.checkCompare(a1, a2) <> that.checkCompare(b1, b2)
      }
    }(Equal[(A, B)].contramap(f))

  /**
   * Constructs an `PartialOrd[B]` given an `PartialOrd[A]` and a function `f` to transform a
   * `B` value into an `A` value. The instance will convert each `B` value into
   * an `A` and compare the `A` values.
   */
  final def contramap[B](f: B => A): PartialOrd[B] =
    PartialOrd.make[B]((b1, b2) => checkCompare(f(b1), f(b2)))(E.contramap(f))

  /**
   * Constructs an `PartialOrd[Either[A, B]]` given an `PartialOrd[A]` and an `PartialOrd[B]`. If
   * one value is `Left` and one value is `Right` it will treat the `Left`
   * value as less than the `Right` value. Otherwise, it will compare the two
   * values.
   */
  final def either[B: Equal](that: => PartialOrd[B]): PartialOrd[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `PartialOrd[C]` given an `PartialOrd[A]`, an `PartialOrd[B]`, and a function `f`
   * to transform a `C` value into an `Either[A, B]`. The instance will convert
   * each `C` value into an `Either[A, B]`. If one value is `Left` and one
   * value is `Right` it will treat the `Left` value as less than the `Right`
   * value. Otherwise, it will compare the two values.
   */
  final def eitherWith[B: Equal, C](that: => PartialOrd[B])(f: C => Either[A, B]): PartialOrd[C] =
    PartialOrd.make[C] { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.compare(a1, a2)
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => that.compare(b1, b2)
      }
    }(Equal[Either[A, B]].contramap(f))

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
  // val antisymmetryLaw1: Laws[PartialOrd] =
  //   new Laws.Law2[PartialOrd]("antisymmetryLaw1") {
  //     def apply[A: PartialOrd: Equal](a1: A, a2: A): TestResult =
  //       ((a1 lessOrEqual a2) && (a2 lessOrEqual a1)) ==> (a1 isEqualTo a2)
  //   }

  /**
   * For all values `a1` and `a2`, if `a1` is greater than or equal to `a2` and
   * `a2` is greater than or equal to `a1` then `a1` is equal to `a2`.
   */
  // val antisymmetryLaw2: Laws[PartialOrd] =
  //   new Laws.Law2[PartialOrd]("antisymmetryLaw2") {
  //     def apply[A: PartialOrd: Equal](a1: A, a2: A): TestResult =
  //       ((a1 greaterOrEqual a2) && (a2 greaterOrEqual a1)) ==> (a1 isEqualTo a2)
  //   }

  /**
   * For all values `a1` and `a2`, iff `a1 =??= a2` is `Ordering.Equals` then `a1 === a2`.
   */
  // val eqConsistencyLaw: Laws[PartialOrd] =
  //   new Laws.Law2[PartialOrd]("eqConsistencyLaw") {
  //     def apply[A: PartialOrd: Equal](a1: A, a2: A): TestResult =
  //       ((a1 =??= a2) isEqualTo Ordering.Equals) <==> ((a1 === a2) isEqualTo true)
  //   }

  /**
   * The set of all laws that instances of `PartialOrd` must satisfy.
   */
  // val laws: Laws[PartialOrd] =
  //   transitivityLaw1 +
  //     transitivityLaw2 +
  //     antisymmetryLaw1 +
  //     antisymmetryLaw2

  /**
   * The `Contravariant` instance for `PartialOrd`.
   */
  // implicit val PartialOrdContravariant: Contravariant[PartialOrd] =
  //   new Contravariant[PartialOrd] {
  //     def contramap[A, B](f: B => A): PartialOrd[A] => PartialOrd[B] =
  //       _.contramap(f)
  //   }

  /**
   * The `IdentityBoth` (and thus `AssociativeBoth`) instance for `PartialOrd`.
   */
  // implicit val PartialOrdIdentityBoth: IdentityBoth[PartialOrd] =
  //   new IdentityBoth[PartialOrd] {
  //     val any: PartialOrd[Any]                                                       =
  //       AnyHashOrd
  //     def both[A, B](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[(A, B)] =
  //       fa.both(fb)
  //   }

  /**
   * The `IdentityEither` (and thus `AssociativeEither`) instance for `PartialOrd`.
   */
  // implicit val PartialOrdIdentityEither: IdentityEither[PartialOrd] =
  //   new IdentityEither[PartialOrd] {
  //     def either[A, B: Equal](fa: => PartialOrd[A], fb: => PartialOrd[B]): PartialOrd[Either[A, B]] =
  //       fa.either(fb)
  //     val none: PartialOrd[Nothing]                                                                 =
  //       NothingPartialOrd
  //   }

  /**
   * Summons an implicit `PartialOrd[A]`.
   */
  def apply[A](implicit ord: PartialOrd[A]): PartialOrd[A] =
    ord

  def default[A: Equal](implicit ord: scala.math.PartialOrdering[A]): PartialOrd[A] =
    make((l, r) => PartialOrdering.fromTryCompare(ord.tryCompare(l, r)))

  /**
   * Constructs an `PartialOrd[A]` from a function. The instance will be optimized to
   * first compare the values for reference equality and then compare the
   * values using the specified function.
   */
  def make[A: Equal](ord: (A, A) => PartialOrdering): PartialOrd[A] =
    new PartialOrd[A] {
      def checkCompare(l: A, r: A): PartialOrdering = ord(l, r)
    }

  val AnyPartialOrd: PartialOrd[Any] =
    make((_: Any, _: Any) => Ordering.Equals)(Equal.AnyEqual)

  implicit val NothingPartialOrd: PartialOrd[Nothing] =
    make[Nothing]((_: Nothing, _: Nothing) => sys.error("nothing.partialord"))(Equal.NothingEqual)

  implicit val UnitPartialOrd: PartialOrd[Unit] =
    make((_, _) => Ordering.Equals)

  implicit val BooleanPartialOrd: PartialOrd[Boolean] =
    default

  implicit val BytePartialOrd: PartialOrd[Byte] =
    default

  implicit val ShortPartialOrd: PartialOrd[Short] =
    default

  implicit val CharPartialOrd: PartialOrd[Char] =
    default

  implicit val DoublePartialOrd: PartialOrd[Double] =
    make((l, r) => Ordering.fromCompare(java.lang.Double.compare(l, r)))

  implicit val FloatPartialOrd: PartialOrd[Float] =
    make((l, r) => Ordering.fromCompare(java.lang.Float.compare(l, r)))

  implicit lazy val FiberIdPartialOrd: PartialOrd[Fiber.Id] =
    PartialOrd[(Long, Long)].contramap[Fiber.Id](fid => (fid.startTimeMillis, fid.seqNumber))

  implicit val IntPartialOrd: PartialOrd[Int] =
    default

  implicit val LongPartialOrd: PartialOrd[Long] =
    default

  implicit def SetPartialOrd[A]: PartialOrd[Set[A]] =
    make((l, r) =>
      if (l == r) Ordering.Equals
      else if (l.subsetOf(r)) Ordering.LessThan
      else if (r.subsetOf(l)) Ordering.GreaterThan
      else PartialOrdering.Incomparable
    )

  implicit val StringPartialOrd: PartialOrd[String] =
    default

  /**
   * Derives an `PartialOrd[Chunk[A]]` given an `PartialOrd[A]`.
   */
  implicit def ChunkPartialOrd[A: PartialOrd: Equal]: PartialOrd[Chunk[A]] =
    make(
      { (l, r) =>
        val j           = l.length
        val k           = r.length
        val PartialOrdA = PartialOrd[A]

        @tailrec
        def loop(i: Int): PartialOrdering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else
            PartialOrdA.compare(l(i), r(i)) match {
              case Ordering.Equals => loop(i + 1)
              case compare         => compare
            }

        loop(0)
      }
    )

  /**
   * Derives an `PartialOrd[F[A]]` given a `Derive[F, PartialOrd]` and an `PartialOrd[A]`.
   */
  implicit def DerivePartialOrd[F[_], A](implicit derive: Derive[F, PartialOrd], ord: PartialOrd[A]): PartialOrd[F[A]] =
    derive.derive(ord)

  /**
   * Derives an `PartialOrd[Either[A, B]]` given an `PartialOrd[A]` and an `PartialOrd[B]`.
   */
  implicit def EitherPartialOrd[A: PartialOrd: Equal, B: PartialOrd: Equal]: PartialOrd[Either[A, B]] =
    make(
      {
        case (Left(a1), Left(a2))   => a1 =??= a2
        case (Left(_), Right(_))    => Ordering.LessThan
        case (Right(_), Left(_))    => Ordering.GreaterThan
        case (Right(b1), Right(b2)) => b1 =??= b2
      }
    )

  /**
   * Derives an `PartialOrd[List[A]]` given an `PartialOrd[A]`.
   */
  implicit def ListPartialOrd[A: PartialOrd: Equal]: PartialOrd[List[A]] = {
    val PartialOrdA = PartialOrd[A]

    @tailrec
    def loop(left: List[A], right: List[A]): PartialOrdering =
      (left, right) match {
        case (Nil, Nil)           => Ordering.Equals
        case (Nil, _)             => Ordering.LessThan
        case (_, Nil)             => Ordering.GreaterThan
        case (h1 :: t1, h2 :: t2) =>
          PartialOrdA.compare(h1, h2) match {
            case Ordering.Equals => loop(t1, t2)
            case compare         => compare
          }
      }

    make((l, r) => loop(l, r))
  }

  /**
   * Derives an `PartialOrd[Map[A, B]]` given an `Equal[B]`.
   * Due to the limitations of Scala's `Map`, this uses object equality on the keys.
   */
  implicit def MapPartialOrd[A, B: Equal]: PartialOrd[Map[A, B]] =
    make(_.compareStrict(_))

  /**
   * Derives an `PartialOrd[NonEmptyChunk[A]]` given an `PartialOrd[A]`.
   */
  implicit def NonEmptyChunkPartialOrd[A: PartialOrd: Equal]: PartialOrd[NonEmptyChunk[A]] =
    PartialOrd[Chunk[A]].contramap(_.toChunk)

  /**
   * Derives an `PartialOrd[Option[A]]` given an `PartialOrd[A]`. `None` will be treated as
   * less than all other values.
   */
  implicit def OptionPartialOrd[A: PartialOrd: Equal]: PartialOrd[Option[A]] =
    UnitPartialOrd.eitherWith(PartialOrd[A]) {
      case None    => Left(())
      case Some(a) => Right(a)
    }

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple2PartialOrd[A: PartialOrd: Equal, B: PartialOrd: Equal]: PartialOrd[(A, B)] =
    make(
      { case ((a1, b1), (a2, b2)) =>
        (a1 =??= a2) <> (b1 =??= b2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple3PartialOrd[A: PartialOrd: Equal, B: PartialOrd: Equal, C: PartialOrd: Equal]
    : PartialOrd[(A, B, C)] =
    make(
      { case ((a1, b1, c1), (a2, b2, c2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple4PartialOrd[A: PartialOrd: Equal, B: PartialOrd: Equal, C: PartialOrd: Equal, D: PartialOrd: Equal]
    : PartialOrd[(A, B, C, D)] =
    make(
      { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple5PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E)] =
    make(
      { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple6PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple7PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple8PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple9PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple10PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple11PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple12PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    make(
      { case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
        (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple13PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple14PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple15PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple16PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple17PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple18PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal,
    R: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple19PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal,
    R: PartialOrd: Equal,
    S: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple20PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal,
    R: PartialOrd: Equal,
    S: PartialOrd: Equal,
    T: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple21PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal,
    R: PartialOrd: Equal,
    S: PartialOrd: Equal,
    T: PartialOrd: Equal,
    U: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2) <> (u1 =??= u2)
      }
    )

  /**
   * Derives an `PartialOrd` for a product type given an `PartialOrd` for each element of
   * the product type.
   */
  implicit def Tuple22PartialOrd[
    A: PartialOrd: Equal,
    B: PartialOrd: Equal,
    C: PartialOrd: Equal,
    D: PartialOrd: Equal,
    E: PartialOrd: Equal,
    F: PartialOrd: Equal,
    G: PartialOrd: Equal,
    H: PartialOrd: Equal,
    I: PartialOrd: Equal,
    J: PartialOrd: Equal,
    K: PartialOrd: Equal,
    L: PartialOrd: Equal,
    M: PartialOrd: Equal,
    N: PartialOrd: Equal,
    O: PartialOrd: Equal,
    P: PartialOrd: Equal,
    Q: PartialOrd: Equal,
    R: PartialOrd: Equal,
    S: PartialOrd: Equal,
    T: PartialOrd: Equal,
    U: PartialOrd: Equal,
    V: PartialOrd: Equal
  ]: PartialOrd[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    make(
      {
        case (
              (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
              (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
            ) =>
          (a1 =??= a2) <> (b1 =??= b2) <> (c1 =??= c2) <> (d1 =??= d2) <> (e1 =??= e2) <> (f1 =??= f2) <> (g1 =??= g2) <> (h1 =??= h2) <> (i1 =??= i2) <> (j1 =??= j2) <> (k1 =??= k2) <> (l1 =??= l2) <> (m1 =??= m2) <> (n1 =??= n2) <> (o1 =??= o2) <> (p1 =??= p2) <> (q1 =??= q2) <> (r1 =??= r2) <> (s1 =??= s2) <> (t1 =??= t2) <> (u1 =??= u2) <> (v1 =??= v2)
      }
    )

  /**
   * Derives an `PartialOrd[Vector[A]]` given an `PartialOrd[A]`.
   */
  implicit def VectorPartialOrd[A: PartialOrd: Equal]: PartialOrd[Vector[A]] =
    make(
      { (l, r) =>
        val j           = l.length
        val k           = r.length
        val PartialOrdA = PartialOrd[A]

        @tailrec
        def loop(i: Int): PartialOrdering =
          if (i == j && i == k) Ordering.Equals
          else if (i == j) Ordering.LessThan
          else if (i == k) Ordering.GreaterThan
          else
            PartialOrdA.compare(l(i), r(i)) match {
              case Ordering.Equals => loop(i + 1)
              case compare         => compare
            }

        loop(0)
      }
    )
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
      ord.compare(l, r) match {
        case Ordering.GreaterThan => true
        case _                    => false
      }

    /**
     * Returns whether this value is greater than or equal to the specified
     * value.
     */
    def >=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      ord.compare(l, r) match {
        case Ordering.GreaterThan => true
        case Ordering.Equals      => true
        case _                    => false
      }

    /**
     * Returns whether this value is less than the specified value.
     */
    def <[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      ord.compare(l, r) match {
        case Ordering.LessThan => true
        case _                 => false
      }

    /**
     * Returns whether this value is less than or equal to the specified
     * value.
     */
    def <=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): Boolean =
      ord.compare(l, r) match {
        case Ordering.LessThan => true
        case Ordering.Equals   => true
        case _                 => false
      }

    /**
     * Returns the result of comparing this value with the specified value.
     */
    def =??=[A1 >: A](r: A1)(implicit ord: PartialOrd[A1]): PartialOrdering = ord.compare(l, r)
  }
}
