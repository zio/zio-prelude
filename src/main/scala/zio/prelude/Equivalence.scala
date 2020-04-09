package zio.prelude

final case class Equivalence[A, B](to: A => B, from: B => A) { self =>
  def >>>[C](that: Equivalence[B, C]): Equivalence[A, C] = self andThen that

  def andThen[C](that: Equivalence[B, C]): Equivalence[A, C] =
    Equivalence(self.to andThen that.to, self.from compose that.from)

  def compose[C](that: Equivalence[C, A]): Equivalence[C, B] = that andThen self

  def flip: Equivalence[B, A] = Equivalence(from, to)

  def toPartialEquivalence: PartialEquivalence[A, B, Nothing, Nothing] =
    PartialEquivalence((a: A) => Right(to(a)), (b: B) => Right(from(b)))
}
object Equivalence {
  def identity[A]: Equivalence[A, A] = Equivalence(Predef.identity[A](_), Predef.identity[A](_))
}
