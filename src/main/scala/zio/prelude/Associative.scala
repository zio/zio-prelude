package zio.prelude

import zio.prelude.coherent.AssociativeEqual
import zio.prelude.newtypes.{ And, First, Last, Max, Min, Or, Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }
import zio.{ Chunk, NonEmptyChunk }

/**
 * The `Associative[A]` type class describes an associative binary operator
 * for a type `A`. For example, addition for integers, and string
 * concatenation for strings.
 *
 * `Associative` is at the top of the hierarchy for abstracting over operations
 * to combine types because while there are some operations that are not
 * associative but do obey other laws, it is generally difficult to combine
 * more than two values in interesting ways with these operators, and thus to
 * build solutions to more complicated problems out of solutions to simpler
 * ones.
 *
 * For example, the mean of two numbers is an operation that is commutative but
 * not associative. However, the lack of associativity is an indication that we
 * can't combine the means of multiple values in an interesting way with this
 * definition. If we attempt to take the mean of three values we always place
 * twice as much weight on one number as the others, which is rarely what we
 * want.
 *
 * If we instead define this operation using a `StatsCounter` object then means
 * can be combined in ways that are associative, commutative, and have an
 * identity element, supporting much more interesting modes of composition.
 */
trait Associative[A] {
  def combine(l: => A, r: => A): A
}

object Associative extends Lawful[AssociativeEqual] {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  val associativityLaw: Laws[AssociativeEqual] =
    new Laws.Law3[AssociativeEqual]("associativityLaw") {
      def apply[A: AssociativeEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
    }

  /**
   * The set of all laws that instances of `Associative` must satisfy.
   */
  val laws: Laws[AssociativeEqual] =
    associativityLaw

  /**
   * Summons an implicit `Associative[A]`.
   */
  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  /**
   * Constructs an `Associative` instance from an associative binary operator.
   */
  def make[A](f: (A, A) => A): Associative[A] =
    (l, r) => f(l, r)

  /**
   * The `Commutative` and `Inverse` instance for the conjunction of `Boolean`
   * values.
   */
  implicit val BooleanConjunctionCommutativeInverse: Commutative[And] with Inverse[And] =
    new Commutative[And] with Inverse[And] {
      def combine(l: => And, r: => And): And = And(l && r)
      val identity: And                      = And(true)
      def inverse(l: => And, r: => And): And = And(l || !r)
    }

  /**
   * The `Commutative` and `Inverse` instance for the disjunction of `Boolean`
   * values.
   */
  implicit val BooleanDisjunctionCommutativeInverse: Commutative[Or] with Inverse[Or] =
    new Commutative[Or] with Inverse[Or] {
      def combine(l: => Or, r: => Or): Or = Or(l || r)
      val identity: Or                    = Or(false)
      def inverse(l: => Or, r: => Or): Or = Or(l && !r)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Boolean`
   * values.
   */
  implicit val BooleanProdCommutativeIdentity: Commutative[Prod[Boolean]] with Identity[Prod[Boolean]] =
    new Commutative[Prod[Boolean]] with Identity[Prod[Boolean]] {
      def combine(l: => Prod[Boolean], r: => Prod[Boolean]): Prod[Boolean] = Prod(l && r)
      val identity: Prod[Boolean]                                          = Prod(true)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Boolean` values.
   */
  implicit val BooleanSumCommutativeInverse: Commutative[Sum[Boolean]] with Inverse[Sum[Boolean]] =
    new Commutative[Sum[Boolean]] with Inverse[Sum[Boolean]] {
      def combine(l: => Sum[Boolean], r: => Sum[Boolean]): Sum[Boolean] = Sum(l || r)
      val identity: Sum[Boolean]                                        = Sum(false)
      def inverse(l: => Sum[Boolean], r: => Sum[Boolean]): Sum[Boolean] = Sum(l && !r)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Byte`
   * values.
   */
  implicit val ByteProdCommutativeIdentity: Commutative[Prod[Byte]] with Identity[Prod[Byte]] =
    new Commutative[Prod[Byte]] with Identity[Prod[Byte]] {
      def combine(l: => Prod[Byte], r: => Prod[Byte]): Prod[Byte] = Prod((l * r).toByte)
      val identity: Prod[Byte]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Byte` values.
   */
  implicit val ByteSumCommutativeInverse: Commutative[Sum[Byte]] with Inverse[Sum[Byte]] =
    new Commutative[Sum[Byte]] with Inverse[Sum[Byte]] {
      def combine(l: => Sum[Byte], r: => Sum[Byte]): Sum[Byte] = Sum((l + r).toByte)
      val identity: Sum[Byte]                                  = Sum(0)
      def inverse(l: => Sum[Byte], r: => Sum[Byte]): Sum[Byte] = Sum((l - r).toByte)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Char`
   * values.
   */
  implicit val CharProdCommutativeIdentity: Commutative[Prod[Char]] with Identity[Prod[Char]] =
    new Commutative[Prod[Char]] with Identity[Prod[Char]] {
      def combine(l: => Prod[Char], r: => Prod[Char]): Prod[Char] = Prod((l * r).toChar)
      val identity: Prod[Char]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Char` values.
   */
  implicit val CharSumCommutativeInverse: Commutative[Sum[Char]] with Inverse[Sum[Char]] =
    new Commutative[Sum[Char]] with Inverse[Sum[Char]] {
      def combine(l: => Sum[Char], r: => Sum[Char]): Sum[Char] = Sum((l + r).toChar)
      val identity: Sum[Char]                                  = Sum(0)
      def inverse(l: => Sum[Char], r: => Sum[Char]): Sum[Char] = Sum((l - r).toChar)
    }

  /**
   * The `Identity` instance for the concatenation of `Chunk[A]` values.
   */
  implicit def ChunkIdentity[A]: Identity[Chunk[A]] =
    Identity.make(Chunk.empty, _ ++ _)

  /**
   * Derives an `Associative[F[A]]` given a `Derive[F, Associative]` and an
   * `Associative[A]`.
   */
  implicit def DeriveAssociative[F[_], A](implicit
    derive: Derive[F, Associative],
    associative: Associative[A]
  ): Associative[F[A]] =
    derive.derive(associative)

  /**
   * The `Commutative` and `Identity` instance for the product of `Double`
   * values.
   */
  implicit val DoubleProdCommutativeIdentity: Commutative[Prod[Double]] with Identity[Prod[Double]] =
    new Commutative[Prod[Double]] with Identity[Prod[Double]] {
      def combine(l: => Prod[Double], r: => Prod[Double]): Prod[Double] = Prod(l * r)
      val identity: Prod[Double]                                        = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Double` values.
   */
  implicit val DoubleSumCommutativeInverse: Commutative[Sum[Double]] with Inverse[Sum[Double]] =
    new Commutative[Sum[Double]] with Inverse[Sum[Double]] {
      def combine(l: => Sum[Double], r: => Sum[Double]): Sum[Double] = Sum(l + r)
      val identity: Sum[Double]                                      = Sum(0)
      def inverse(l: => Sum[Double], r: => Sum[Double]): Sum[Double] = Sum(l - r)
    }

  /**
   * Derives an `Associative[Either[E, A]]` given an `Associative[A]`.
   */
  implicit def EitherAssociative[E, A: Associative]: Associative[Either[E, A]] =
    make {
      case (Left(l), _)         => Left(l)
      case (_, Left(r))         => Left(r)
      case (Right(l), Right(r)) => Right(l <> r)
    }

  /**
   * The `Associative` instance for the first of `A` values.
   */
  implicit def FirstAssociative[A]: Associative[First[A]] =
    make((l: First[A], _: First[A]) => l)

  /**
   * The `Commutative` and `Identity` instance for the product of `Float`
   * values.
   */
  implicit val FloatProdCommutativeIdentity: Commutative[Prod[Float]] with Identity[Prod[Float]] =
    new Commutative[Prod[Float]] with Identity[Prod[Float]] {
      def combine(l: => Prod[Float], r: => Prod[Float]): Prod[Float] = Prod(l * r)
      val identity: Prod[Float]                                      = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Float` values.
   */
  implicit val FloatSumCommutativeInverse: Commutative[Sum[Float]] with Inverse[Sum[Float]] =
    new Commutative[Sum[Float]] with Inverse[Sum[Float]] {
      def combine(l: => Sum[Float], r: => Sum[Float]): Sum[Float] = Sum(l + r)
      val identity: Sum[Float]                                    = Sum(0)
      def inverse(l: => Sum[Float], r: => Sum[Float]): Sum[Float] = Sum(l - r)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Int` values.
   */
  implicit val IntProdCommutativeIdentity: Commutative[Prod[Int]] with Identity[Prod[Int]] =
    new Commutative[Prod[Int]] with Identity[Prod[Int]] {
      def combine(l: => Prod[Int], r: => Prod[Int]): Prod[Int] = Prod(l * r)
      val identity: Prod[Int]                                  = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Int` values.
   */
  implicit val IntSumCommutativeInverse: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
    new Commutative[Sum[Int]] with Inverse[Sum[Int]] {
      def combine(l: => Sum[Int], r: => Sum[Int]): Sum[Int] = Sum(l + r)
      val identity: Sum[Int]                                = Sum(0)
      def inverse(l: => Sum[Int], r: => Sum[Int]): Sum[Int] = Sum(l - r)
    }

  /**
   * The `Associative` instance for the last of `A` values.
   */
  implicit def LastAssociative[A]: Associative[Last[A]] =
    make((_: Last[A], r: Last[A]) => r)

  /**
   * The `Identity` instance for the concatenation of `List[A]` values.
   */
  implicit def ListIdentity[A]: Identity[List[A]] =
    Identity.make[List[A]](Nil, _ ++ _)

  /**
   * The `Commutative` and `Identity` instance for the product of `Long`
   * values.
   */
  implicit val LongProdCommutativeIdentity: Commutative[Prod[Long]] with Identity[Prod[Long]] =
    new Commutative[Prod[Long]] with Identity[Prod[Long]] {
      def combine(l: => Prod[Long], r: => Prod[Long]): Prod[Long] = Prod(l * r)
      val identity: Prod[Long]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Long` values.
   */
  implicit val LongSumCommutativeInverse: Commutative[Sum[Long]] with Inverse[Sum[Long]] =
    new Commutative[Sum[Long]] with Inverse[Sum[Long]] {
      def combine(l: => Sum[Long], r: => Sum[Long]): Sum[Long] = Sum(l + r)
      val identity: Sum[Long]                                  = Sum(0)
      def inverse(l: => Sum[Long], r: => Sum[Long]): Sum[Long] = Sum(l - r)
    }

  /**
   * Derives an `Identity[Map[K, V]]` given an `Associative[V]`.
   */
  implicit def MapIdentity[K, V: Associative]: Identity[Map[K, V]] =
    new Identity[Map[K, V]] {
      def identity: Map[K, V] = Map()

      def combine(l: => Map[K, V], r: => Map[K, V]): Map[K, V] =
        r.foldLeft(l) { case (map, (k, v)) =>
          map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  /**
   * The `Commutative` instance for the maximum of `A` values for which an
   * `Ord` is defined.
   */
  implicit def MaxCommutative[A: Ord]: Commutative[Max[A]] =
    Commutative.make((l: Max[A], r: Max[A]) => if (l >= r) l else r)

  /**
   * The `Commutative` instance for the minimum of `A` values for which an
   * `Ord` is defined.
   */
  implicit def MinCommutative[A: Ord]: Commutative[Min[A]] =
    Commutative.make((l: Min[A], r: Min[A]) => if (l <= r) l else r)

  /**
   * The `Associative` instance for the concatenation of `NonEmptyChunk[A]`
   * values.
   */
  implicit def NonEmptyChunkAssociative[A]: Associative[NonEmptyChunk[A]] =
    make(_ ++ _)

  /**
   * Derives an `Identity[Option[A]]` given an `Associative[A]`.
   */
  implicit def OptionIdentity[A: Associative]: Identity[Option[A]] =
    Identity.make(
      None,
      {
        case (Some(l), Some(r)) => Some(l <> r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case _                  => None
      }
    )

  /**
   * The `Commutative` and `Inverse` instance for the union of `Set[A]`
   * values.
   */
  implicit def SetCommutativeInverse[A]: Commutative[Set[A]] with Inverse[Set[A]] =
    new Commutative[Set[A]] with Inverse[Set[A]] {
      def combine(l: => Set[A], r: => Set[A]): Set[A] = l | r
      val identity: Set[A]                            = Set.empty
      def inverse(l: => Set[A], r: => Set[A]): Set[A] = l &~ r
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Short`
   * values.
   */
  implicit val ShortProdCommutativeIdentity: Commutative[Prod[Short]] with Identity[Prod[Short]] =
    new Commutative[Prod[Short]] with Identity[Prod[Short]] {
      def combine(l: => Prod[Short], r: => Prod[Short]): Prod[Short] = Prod((l * r).toShort)
      val identity: Prod[Short]                                      = Prod(1)
    }

  /**
   * The `Commutative` and `Identity` instance for the sum of `Short` values.
   */
  implicit val ShortSumCommutativeIdentity: Commutative[Sum[Short]] with Inverse[Sum[Short]] =
    new Commutative[Sum[Short]] with Inverse[Sum[Short]] {
      def combine(l: => Sum[Short], r: => Sum[Short]): Sum[Short] = Sum((l + r).toShort)
      val identity: Sum[Short]                                    = Sum(0)
      def inverse(l: => Sum[Short], r: => Sum[Short]): Sum[Short] = Sum((l - r).toShort)
    }

  /**
   * The `Identity` instance for the concatenation of `String` values.
   */
  implicit val StringIdentity: Identity[String] =
    Identity.make("", _ + _)

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple2Associative[A: Associative, B: Associative]: Associative[(A, B)] =
    make { case ((a1, b1), (a2, b2)) =>
      (a1 <> a2, b1 <> b2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple3Associative[A: Associative, B: Associative, C: Associative]: Associative[(A, B, C)] =
    make { case ((a1, b1, c1), (a2, b2, c2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple4Associative[A: Associative, B: Associative, C: Associative, D: Associative]
    : Associative[(A, B, C, D)] =
    make { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple5Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative]
    : Associative[(A, B, C, D, E)] =
    make { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
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

  /**
   * The `Identity` instance for the concatenation of `Vector[A]` values.
   */
  implicit def VectorIdentity[A]: Identity[Vector[A]] =
    Identity.make(Vector.empty, _ ++ _)
}

trait AssociativeSyntax {

  /**
   * Provides infix syntax for combining two values with an associative
   * operation.
   */
  implicit class AssociativeOps[A](l: A) {

    /**
     * A symbolic alias for `combine`.
     */
    def <>(r: => A)(implicit associative: Associative[A]): A =
      associative.combine(l, r)

    /**
     * Associatively combines this value with the specified value
     */
    def combine(r: => A)(implicit associative: Associative[A]): A =
      associative.combine(l, r)
  }
}
