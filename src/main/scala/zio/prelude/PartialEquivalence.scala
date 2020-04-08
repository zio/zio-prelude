package zio.prelude

final case class PartialEquivalence[A, B, +E1, +E2](to: A => Either[E1, B], from: B => Either[E2, A]) {
  def canonicalLeft(a: A): Option[A] = to(a).toOption.flatMap(b => from(b).toOption)

  def canonicalRight(b: B): Option[B] = from(b).toOption.flatMap(a => to(a).toOption)
}

// TODO?
// type Equivalence[A, B] = PartialEquivalence[A, B, Nothing, Nothing]
