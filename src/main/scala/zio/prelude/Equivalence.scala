package zio.prelude

import zio.test.TestResult
import zio.test.laws._

/**
 * An `Equivalence[A, B]` defines an equivalence between two types `A` and `B`.
 * These types represent different ways to store the same information.
 *
 * For example, a `List[Byte]` is equivalent to a `Vector[Byte]`. Similarly, a
 * `List[Char]` is equivalent to a `String`.
 *
 * Equivalences are symmetrical. So if `A` is equivalent to `B`, then `B` is
 * equivalent to `A`.
 *
 * @param to   A function that converts an `A` into a `B`.
 * @param from A function that converts a `B` into an `A`.
 */
final case class Equivalence[A, B](to: A => B, from: B => A) { self =>

  /**
   * Composes this equivalence with the specified equivalence.
   */
  def >>>[C](that: Equivalence[B, C]): Equivalence[A, C] = self andThen that

  /**
   * A named method for `>>>`.
   */
  def andThen[C](that: Equivalence[B, C]): Equivalence[A, C] =
    Equivalence(self.to andThen that.to, self.from compose that.from)

  def compose[C](that: Equivalence[C, A]): Equivalence[C, B] = that andThen self

  /**
   * Flips this equivalence around.
   */
  def flip: Equivalence[B, A] = Equivalence(from, to)

  /**
   * Converts this equivalence to a partial equivalence that cannot fail in
   * either direction.
   */
  def toPartialEquivalence: PartialEquivalence[A, B, Nothing, Nothing] =
    PartialEquivalence((a: A) => Right(to(a)), (b: B) => Right(from(b)))
}

object Equivalence extends Lawful2[Equivalence, Equal, Equal] {

  val leftIdentity: Laws2[Equivalence, Equal, AnyF] =
    new Laws2.Law1Left[Equivalence, Equal, AnyF]("leftIdentity") {
      def apply[A: Equal, B: AnyF](a: A)(implicit equivalence: Equivalence[A, B]): TestResult =
        equivalence.from(equivalence.to(a)) <-> a
    }

  val rightIdentity: Laws2[Equivalence, AnyF, Equal] =
    new Laws2.Law1Right[Equivalence, AnyF, Equal]("rightIdentity") {
      def apply[A: AnyF, B: Equal](b: B)(implicit equivalence: Equivalence[A, B]): TestResult =
        equivalence.to(equivalence.from(b)) <-> b
    }

  val laws: Laws2[Equivalence, Equal, Equal] =
    leftIdentity + rightIdentity

  /**
   * Constructs the identity equivalence, which just says that any type is
   * equivalent to itself and is trivially true.
   */
  def identity[A]: Equivalence[A, A] = Equivalence(Predef.identity[A], Predef.identity[A])

  /**
   * Constructs an equivalence between a right-associated nested tuple, and a
   * left-associated nested tuple.
   */
  def tuple[A, B, C]: Equivalence[(A, (B, C)), ((A, B), C)] =
    Equivalence(
      {
        case (a, (b, c)) => ((a, b), c)
      },
      {
        case ((a, b), c) => (a, (b, c))
      }
    )

  def tupleFlip[A, B]: Equivalence[(A, B), (B, A)] =
    Equivalence(
      {
        case (a, b) => (b, a)
      },
      {
        case (b, a) => (a, b)
      }
    )

  def tupleAny[A]: Equivalence[(A, Any), A] =
    Equivalence(_._1, a => (a, ()))

  /**
   * Constructs an equivalence between a right-associated nested either, and a
   * left-associated nested either.
   */
  def either[A, B, C]: Equivalence[Either[A, Either[B, C]], Either[Either[A, B], C]] =
    Equivalence(
      {
        case Left(a)         => Left(Left(a))
        case Right(Left(b))  => Left(Right(b))
        case Right(Right(c)) => Right(c)
      },
      {
        case Left(Left(a))  => Left(a)
        case Left(Right(b)) => Right(Left(b))
        case Right(c)       => Right(Right(c))
      }
    )

  def eitherFlip[A, B]: Equivalence[Either[A, B], Either[B, A]] =
    Equivalence(
      {
        case Left(a)  => Right(a)
        case Right(b) => Left(b)
      },
      {
        case Right(a) => Left(a)
        case Left(b)  => Right(b)
      }
    )

  def eitherNothing[A]: Equivalence[Either[A, Nothing], A] =
    Equivalence(
      {
        case Left(a)        => a
        case Right(nothing) => nothing
      },
      a => Left(a)
    )

}
