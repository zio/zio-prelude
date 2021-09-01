package zio.prelude.refined

final case class QuotedAssertion[A, T](assertion: Refinement[A]) { self =>
  def unsafeApply(a: A): T = a.asInstanceOf[T]

  def unwrap(t: T): A = t.asInstanceOf[A]
}

extension [A, T](inline self: QuotedAssertion[A, T]) {
  transparent inline def apply(inline a: A): Any = Refined.make(self, a)
}
