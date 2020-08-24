package zio.prelude

import com.github.ghik.silencer.silent

@silent("Unused import")
final case class PartialEquivalence[A, B, +E1, +E2](to: A => Either[E1, B], from: B => Either[E2, A]) { self =>
  import PartialEquivalence._
  import zio._ // for zio.EitherCompat

  def >>>[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    self andThen that

  def andThen[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[B, C, E3, E4]): PartialEquivalence[A, C, E3, E4] =
    PartialEquivalence(self.to andThen (_.flatMap(that.to)), that.from andThen (_.flatMap(self.from)))

  def canonicalLeft(a: A): Option[A] = canonicalLeftOrError[Any](a).fold(_ => None, Some(_))

  def canonicalLeftOrError[E](a: A)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, A] =
    to(a).left.map(ev1).flatMap(b => from(b).left.map(ev2))

  def canonicalRight(b: B): Option[B] = canonicalRightOrError[Any](b).fold(_ => None, Some(_))

  def canonicalRightOrError[E](b: B)(implicit ev1: E1 <:< E, ev2: E2 <:< E): Either[E, B] =
    from(b).left.map(ev2).flatMap(a => to(a).left.map(ev1))

  def compose[C, E3 >: E1, E4 >: E2](that: PartialEquivalence[C, A, E3, E4]): PartialEquivalence[C, B, E3, E4] =
    that andThen self

  def flip: PartialEquivalence[B, A, E2, E1] = PartialEquivalence(from, to)

  def toEquivalence(implicit ev1: E1 <:< Nothing, ev2: E2 <:< Nothing): Equivalence[A, B] =
    Equivalence((a: A) => toRight(to(a).left.map(ev1)), (b: B) => toRight(from(b).left.map(ev2)))
}

object PartialEquivalence {
  def identity[A]: PartialEquivalence[A, A, Nothing, Nothing] = Equivalence.identity[A].toPartialEquivalence

  private def toRight[A](either: Either[Nothing, A]): A = either.fold(Predef.identity, Predef.identity)
}
