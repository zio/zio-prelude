package zio.prelude

import zio.Exit.{ Failure, Success }
import scala.annotation.implicitNotFound
import zio.{ Cause, Chunk, Exit, NonEmptyChunk }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Equal[A]` provides implicit evidence that two values of type `A` can be
 * compared for equality.
 */
@implicitNotFound("No implicit Equal defined for ${A}.")
trait Equal[-A] { self =>

  /**
   * Returns whether two values of type `A` are equal.
   */
  final def equal(l: A, r: A): Boolean =
    Equal.refEq(l, r) || checkEqual(l, r)

  /**
   * Returns whether two values of type `A` are equal.
   */
  protected def checkEqual(l: A, r: A): Boolean

  /**
   * Constructs an `Equal[(A, B)]` given an `Equal[A]` and `Equal[B]` by first
   * comparing the `A` values for equality and then comparing the `B` values
   * for equality, if necessary.
   */
  final def both[B](that: => Equal[B]): Equal[(A, B)] =
    bothWith(that)(identity)

  /**
   * Constructs an `Equal[C]` given an `Equal[A]`, an `Equal[B]` and a
   * function `f` to transform a `C` value into an `(A, B)`. The instance
   * will convert each `C` value into an `(A, B)`, compare the `A` values for
   * equality, and then compare the `B` values for equality if necessary.
   */
  final def bothWith[B, C](that: => Equal[B])(f: C => (A, B)): Equal[C] =
    Equal.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.equal(a1, a2) && that.equal(b1, b2)
      }
    }

  /**
   * Constructs an `Equal[B]` given an `Equal[A]` and a function `f` to
   * transform a `B` value into an `A` value. The instance will convert each
   * `B` value into an `A` and the compare the `A` values for equality.
   */
  def contramap[B](f: B => A): Equal[B] =
    Equal.make((b1, b2) => equal(f(b1), f(b2)))

  /**
   * Constructs an `Equal[Either[A, B]]` given an `Equal[A]` and an
   * `Equal[B]`. The instance will compare the `Either[A, B]` values and if
   * both are `Right` or `Left` compare them for equality.
   */
  final def either[B](that: => Equal[B]): Equal[Either[A, B]] =
    eitherWith(that)(identity)

  /**
   * Constructs an `Equal[C]` given an `Equal[A]`, an `Equal[B]`, and a
   * function `f` to transform a `C` value into an `Either[A, B]`. The
   * instance will convert each `C` value into an `Either[A, B]` and then
   * if both are `Right` or `Left` compare them for equality.
   */
  final def eitherWith[B, C](that: => Equal[B])(f: C => Either[A, B]): Equal[C] =
    Equal.make { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.equal(a1, a2)
        case (Right(b1), Right(b2)) => that.equal(b1, b2)
        case _                      => false
      }
    }

  /**
   * Returns whether two values of type `A` are not equal.
   */
  final def notEqual(l: A, r: A): Boolean =
    !equal(l, r)
}

object Equal extends Lawful[Equal] {

  /**
   * For all values `a1`, `a1` is equal to `a1`.
   */
  val reflexiveLaw: Laws.Law1[Equal] =
    new Laws.Law1[Equal]("reflexiveLaw") {
      def apply[A: Equal](a1: A): TestResult =
        a1 <-> a1
    }

  /**
   * For all values `a1` and `a2`, if `a1` is equal to `a2` then `a2` is equal
   * to `a1`.
   */
  val symmetryLaw: Laws.Law2[Equal] =
    new Laws.Law2[Equal]("symmetryLaw") {
      def apply[A: Equal](a1: A, a2: A): TestResult =
        (a1 <-> a2) ==> (a2 <-> a1)
    }

  /**
   * For all values `a1`, `a2`, and `a3`, if `a1` is equal to `a2` and `a2` is
   * equal `a3`, then `a1` is equal to `a3`.
   */
  val transitivityLaw: Laws.Law3[Equal] =
    new Laws.Law3[Equal]("transitivityLaw") {
      def apply[A: Equal](a1: A, a2: A, a3: A): TestResult =
        ((a1 <-> a2) && (a2 <-> a3)) ==> (a1 <-> a3)
    }

  /**
   * The set of all laws that instances of `Equal` must satisfy.
   */
  val laws: Laws[Equal] =
    reflexiveLaw + symmetryLaw + transitivityLaw

  /**
   * The `AssociativeBoth` instance for `Equal`.
   */
  implicit val EqualAssociativeBoth: AssociativeBoth[Equal] =
    new AssociativeBoth[Equal] {
      def both[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `AssociativeEither` instance for `Equal`.
   */
  implicit val EqualAssociativeEither: AssociativeEither[Equal] =
    new AssociativeEither[Equal] {
      def either[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[Either[A, B]] =
        fa.either(fb)
    }

  /**
   * The `CommutativeBoth` instance for `Equal`.
   */
  implicit val EqualCommutativeBoth: CommutativeBoth[Equal] =
    new CommutativeBoth[Equal] {
      def both[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `CommutativeEither` instance for `Equal`.
   */
  implicit val EqualCommutativeEither: CommutativeEither[Equal] =
    new CommutativeEither[Equal] {
      def either[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[Either[A, B]] =
        fa.either(fb)
    }

  /**
   * The `Contravariant` instance for `Equal`.
   */
  implicit val EqualContravariant: Contravariant[Equal] =
    new Contravariant[Equal] {
      def contramap[A, B](f: B => A): Equal[A] => Equal[B] =
        _.contramap(f)
    }

  /**
   * The `IdentityBoth` instance for `Equal`.
   */
  implicit val EqualIdentityBoth: IdentityBoth[Equal] =
    new IdentityBoth[Equal] {
      val any: Equal[Any] =
        AnyEqual
      def both[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[(A, B)] =
        fa.both(fb)
    }

  /**
   * The `IdentityEither` instance for `Equal`.
   */
  implicit val EqualIdentityEither: IdentityEither[Equal] =
    new IdentityEither[Equal] {
      def either[A, B](fa: => Equal[A], fb: => Equal[B]): Equal[Either[A, B]] =
        fa.either(fb)
      val none: Equal[Nothing] =
        NothingEqual
    }

  /**
   * Summons an implicit `Equal[A]`.
   */
  def apply[A](implicit equal: Equal[A]): Equal[A] =
    equal

  /**
   * Constructs an `Equal[A]` from a function. The instance will be optimized
   * to first compare the values for reference equality and then compare the
   * values for value equality.
   */
  def make[A](equal: (A, A) => Boolean): Equal[A] =
    (l, r) => refEq(l, r) || equal(l, r)

  /**
   * Constructs an `Equal[A]` that uses the default notion of equality
   * embodied in the implementation of `equals` for values of type `A`.
   */
  def default[A]: Equal[A] =
    make(_ == _)

  /**
   * Equality for `Any` values. Note that since values of type `Any` contain
   * no information, all values of type `Any` can be treated as equal to each
   * other.
   */
  val AnyEqual: Equal[Any] =
    make((_, _) => true)

  /**
   * Equality for `Boolean` values.
   */
  implicit val BooleanEqual: Equal[Boolean] =
    default

  /**
   * Equality for `Byte` values.
   */
  implicit val ByteEqual: Equal[Byte] =
    default

  /**
   * Equality for `Short` values.
   */
  implicit val ShortEqual: Equal[Short] =
    default

  /**
   * Equality for `Char` values.
   */
  implicit val CharEqual: Equal[Char] =
    default

  /**
   * Derives an `Equal[Chunk[A]]` given an `Equal[A]`.
   */
  implicit def ChunkEqual[A: Equal]: Equal[Chunk[A]] =
    make(_.corresponds(_)(_ === _))

  /**
   * Equality for `Double` values. Note that to honor the contract that a
   * value is always equal to itself, comparing `Double.NaN` with itself will
   * return `true`, which is different from the behavior of `Double#equals`.
   */
  implicit val DoubleEqual: Equal[Double] =
    make { (n1, n2) =>
      if (n1.isNaN && n2.isNaN) true
      else n1 == n2
    }

  /**
   * Derives an `Equal[Either[A, B]]` given an `Equal[A]` and an `Equal[B]`.
   */
  implicit def EitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] =
    Equal[A] either Equal[B]

  /**
   * Derives an `Equal[F[A]]` given an `EqualF[F]` and an `Equal[A]`.
   */
  implicit def EqualFEqual[F[_]: EqualF, A: Equal]: Equal[F[A]] =
    EqualF[F].deriveEqual(Equal[A])

  /**
   * Equality for `Float` values. Note that to honor the contract that a
   * value is always equal to itself, comparing `Float.NaN` with itself will
   * return `true`, which is different from the behavior of `Float#equals`.
   */
  implicit val FloatEqual: Equal[Float] =
    make { (n1, n2) =>
      if (n1.isNaN && n2.isNaN) true
      else n1 == n2
    }

  /**
   * Equality for `Int` values.
   */
  implicit val IntEqual: Equal[Int] =
    default

  /**
   * Derives an `Equal[List[A]]` given an `Equal[A]`.
   */
  implicit def ListEqual[A: Equal]: Equal[List[A]] =
    make(_.corresponds(_)(_ === _))

  /**
   * Equality for `Long` values.
   */
  implicit val LongEqual: Equal[Long] =
    default

  /**
   * Derives an `Equal[Map[A, B]]` given an `Equal[B]`. Due to the limitations
   * of Scala's `Map`, this uses object equality and hash code on the keys.
   */
  implicit def MapEqual[A, B: Equal]: Equal[Map[A, B]] =
    make { (map1, map2) =>
      map1.size == map2.size &&
      map1.forall { case (key, value) => map2.get(key).fold(false)(_ === value) }
    }

  /**
   * Derives an `Equal[NonEmptyChunk[A]]` given an `Equal[A]`.
   */
  implicit def NonEmptyChunkEqual[A: Equal]: Equal[NonEmptyChunk[A]] =
    Equal[Chunk[A]].contramap(_.toChunk)

  /**
   * Equality for `Nothing` values. Note that since there are not values of
   * type `Nothing` the `equals` method of this instance can never be called
   * but it can be useful in deriving instances for more complex types.
   */
  implicit val NothingEqual: Equal[Nothing] =
    default

  /**
   * Derives an `Equal[Option[A]]` given an `Equal[A]`.
   */
  implicit def OptionEqual[A: Equal]: Equal[Option[A]] =
    make {
      case (None, None)         => true
      case (Some(a1), Some(a2)) => a1 === a2
      case _                    => false
    }

  /**
   * Equality for `Set[A]` values. Due to the limitations of Scala's `Set`,
   * this uses object equality and hash code on the elements.
   */
  implicit def SetEqual[A]: Equal[Set[A]] =
    default

  /**
   * Equality for `String` values.
   */
  implicit val StringEqual: Equal[String] =
    default

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple2Equal[A: Equal, B: Equal]: Equal[(A, B)] =
    Equal[A] both Equal[B]

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple3Equal[A: Equal, B: Equal, C: Equal]: Equal[(A, B, C)] =
    make {
      case ((a1, b1, c1), (a2, b2, c2)) => a1 === a2 && b1 === b2 && c1 === c2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple4Equal[A: Equal, B: Equal, C: Equal, D: Equal]: Equal[(A, B, C, D)] =
    make {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple5Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]: Equal[(A, B, C, D, E)] =
    make {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple6Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]: Equal[(A, B, C, D, E, F)] =
    make {
      case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple7Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]
    : Equal[(A, B, C, D, E, F, G)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple8Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal, H: Equal]
    : Equal[(A, B, C, D, E, F, G, H)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple9Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal, H: Equal, I: Equal]
    : Equal[(A, B, C, D, E, F, G, H, I)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple10Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple11Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple12Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    make {
      case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple13Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple14Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple15Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple16Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple17Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple18Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple19Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple20Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple21Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal,
    U: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2 && u1 === u2
    }

  /**
   * Derives an `Equal` for a product type given an `Equal` for each element of
   * the product type.
   */
  implicit def Tuple22Equal[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal,
    U: Equal,
    V: Equal
  ]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    make {
      case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
          ) =>
        a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2 && h1 === h2 && i1 === i2 && j1 === j2 && k1 === k2 && l1 === l2 && m1 === m2 && n1 === n2 && o1 === o2 && p1 === p2 && q1 === q2 && r1 === r2 && s1 === s2 && t1 === t2 && u1 === u2 && v1 === v2
    }

  /**
   * Equality for `Unit` values. Since there is only one `Unit` value all
   * equality comparisons will always be true.
   */
  implicit val UnitEqual: Equal[Unit] =
    make((_, _) => true)

  /**
   * Derives an `Equal[Vector[A]]` given an `Equal[A]`.
   */
  implicit def VectorEqual[A: Equal]: Equal[Vector[A]] =
    make(_.corresponds(_)(_ === _))

  /**
   * Derives an `Equal[Cause[A]]` given an `Equal[A]`.
   */
  implicit def CauseEqual[A: Equal]: Equal[Cause[A]] =
    make(_.contains(_))

  /**
   * Derives an `Equal[Exit[E, A]]` given an `Equal[A]` and `Equal[B]`.
   */
  implicit def ExitEqual[E: Equal, A: Equal]: Equal[Exit[E, A]] =
    make {
      case (Success(a), Success(b))  => a === b
      case (Failure(c), Failure(c1)) => c === c1
      case _                         => false
    }

  /**
   * Returns whether two values refer to the same location in memory.
   */
  private[prelude] def refEq[A](l: A, r: A): Boolean =
    l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]
}

trait EqualSyntax {

  /**
   * Provides infix syntax for comparing two values for equality.
   */
  implicit class EqualOps[A](l: A) {

    /**
     * Returns whether this value and the specified value are equal.
     */
    def ===[A1 >: A](r: A1)(implicit equal: Equal[A1]): Boolean =
      equal.equal(l, r)

    /**
     * Returns whether this value and the specified value are not equal.
     */
    def !==[A1 >: A](r: A1)(implicit equal: Equal[A1]): Boolean =
      equal.notEqual(l, r)
  }
}
