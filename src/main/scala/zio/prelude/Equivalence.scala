package zio.prelude

final case class Equivalence[A, B](to: A => B, from: B => A) {
  def flip: Equivalence[B, A] = Equivalence(from, to)
}
