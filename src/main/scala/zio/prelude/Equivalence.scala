package zio.prelude

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
object Equivalence {

  /**
   * Constructs the identity equivalence, which just says that any type is
   * equivalent to itself and is trivially true.
   */
  def identity[A]: Equivalence[A, A] = Equivalence(Predef.identity[A](_), Predef.identity[A](_))

  def tuple[A, B, C]: Equivalence[(A, (B, C)), ((A, B), C)] =
    Equivalence({
      case (a, (b, c)) => ((a, b), c)
    }, {
      case ((a, b), c) => (a, (b, c))
    })
}
