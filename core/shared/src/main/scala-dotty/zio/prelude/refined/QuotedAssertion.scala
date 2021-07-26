package zio.prelude.refined

final case class QuotedAssertion[-A, T](assertion: Assertion[A])

extension [A, T](inline quotedAssertion: QuotedAssertion[A, T]) {
  transparent inline def apply(inline a: A): Any = Refined.make(quotedAssertion, a)
}
