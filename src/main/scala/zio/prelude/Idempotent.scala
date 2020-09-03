package zio.prelude

import zio.prelude.coherent.EqualIdempotent
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Idempotent` type class describes a binary operator for a type `A` that
 * is both commutative and also has an identity element. This means that `a <> a` is equal to
 * `a` for all values `a`. Examples of idempotent operations
 * include union of sets, but not addition of integers.
 *
 * Idempotent operators are useful because combining  the values with an idempotent
 * operation results in the same value regardless of the number of values
 * are combined, allowing us to optimize out unnecessary combinations of the same values.
 */
trait Idempotent[A] extends Commutative[A] with Identity[A] { self =>
  def optimize(elems: Iterable[A]): Set[A] = elems.toSet
}

object Idempotent extends Lawful[EqualIdempotent] {

  /**
   * The idempotent law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * a === a
   * }}}
   */
  val idempotentLaw: Laws[EqualIdempotent] =
    new Laws.Law1[EqualIdempotent]("idempotentLaw") {
      def apply[A: EqualIdempotent](a: A): TestResult =
        (a <> a) <-> a
    }

  /**
   * The set of all laws that instances of `Idempotent` must satisfy.
   */
  val laws: Laws[EqualIdempotent] =
    idempotentLaw + Commutative.laws + Identity.laws

  /**
   * Summons an implicit `Idempotent[A]`.
   */
  def apply[A](implicit idempotent: Idempotent[A]): Idempotent[A] = idempotent

  /**
   * Constructs a `Idempotent` instance from a commutative binary operator and identity element.
   */
  def make[A](f: (A, A) => A, identity0: A): Idempotent[A] = new Idempotent[A] {
    override def identity: A                  = identity0
    override def combine(l: => A, r: => A): A = f(l, r)
  }

  /**
   * Constructs an `Idempotent` instance from an identity instance.
   */
  def makeFrom[A](identity: Identity[A]): Idempotent[A] =
    make((l, r) => identity.combine(l, r), identity.identity)

  /**
   * Derives a `Idempotent[F[A]]` given a `Derive[F, Idempotent]` and a
   * `Idempotent[A]`.
   */
  implicit def DeriveIdempotent[F[_], A](
    implicit derive: Derive[F, Idempotent],
    idempotent: Idempotent[A]
  ): Idempotent[F[A]] =
    derive.derive(idempotent)

  /**
   * Derives a `Idempotent[Map[K, V]]` given a `Idempotent[V]`.
   */
  implicit def MapIdempotent[K, V: Idempotent]: Idempotent[Map[K, V]] =
    makeFrom(Associative.MapIdentity)

  /**
   * Derives a `Idempotent[Option[A]]` given a `Idempotent[A]`
   */
  implicit def OptionIdempotent[A: Idempotent]: Idempotent[Option[A]] =
    makeFrom(Associative.OptionIdentity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple2Idempotent[A: Idempotent, B: Idempotent]: Idempotent[(A, B)] =
    makeFrom(Identity.Tuple2Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple3Idempotent[A: Idempotent, B: Idempotent, C: Idempotent]: Idempotent[(A, B, C)] =
    makeFrom(Identity.Tuple3Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple4Idempotent[A: Idempotent, B: Idempotent, C: Idempotent, D: Idempotent]: Idempotent[(A, B, C, D)] =
    makeFrom(Identity.Tuple4Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple5Idempotent[A: Idempotent, B: Idempotent, C: Idempotent, D: Idempotent, E: Idempotent]
    : Idempotent[(A, B, C, D, E)] =
    makeFrom(Identity.Tuple5Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple6Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent
  ]: Idempotent[(A, B, C, D, E, F)] =
    makeFrom(Identity.Tuple6Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple7Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G)] =
    makeFrom(Identity.Tuple7Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple8Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H)] =
    makeFrom(Identity.Tuple8Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple9Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I)] =
    makeFrom(Identity.Tuple9Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple10Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J)] =
    makeFrom(Identity.Tuple10Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple11Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K)] =
    makeFrom(Identity.Tuple11Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple12Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    makeFrom(Identity.Tuple12Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple13Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    makeFrom(Identity.Tuple13Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple14Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    makeFrom(Identity.Tuple14Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple15Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    makeFrom(Identity.Tuple15Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple16Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    makeFrom(Identity.Tuple16Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple17Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    makeFrom(Identity.Tuple17Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple18Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    makeFrom(Identity.Tuple18Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple19Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    makeFrom(Identity.Tuple19Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple20Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    makeFrom(Identity.Tuple20Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple21Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent,
    U: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    makeFrom(Identity.Tuple21Identity)

  /**
   * Derives a `Idempotent` for a product type given a `Idempotent` for each
   * element of the product type.
   */
  implicit def Tuple22Idempotent[
    A: Idempotent,
    B: Idempotent,
    C: Idempotent,
    D: Idempotent,
    E: Idempotent,
    F: Idempotent,
    G: Idempotent,
    H: Idempotent,
    I: Idempotent,
    J: Idempotent,
    K: Idempotent,
    L: Idempotent,
    M: Idempotent,
    N: Idempotent,
    O: Idempotent,
    P: Idempotent,
    Q: Idempotent,
    R: Idempotent,
    S: Idempotent,
    T: Idempotent,
    U: Idempotent,
    V: Idempotent
  ]: Idempotent[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    makeFrom(Identity.Tuple22Identity)
}
