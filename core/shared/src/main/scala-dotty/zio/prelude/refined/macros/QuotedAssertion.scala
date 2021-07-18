package zio.prelude.refined.macros

import zio.prelude.refined.Assertion

final case class QuotedAssertion[-A, T](assertion: Assertion[A])

extension [A, T](inline quotedAssertion: QuotedAssertion[A, T]) {
  transparent inline def apply(inline a: A): Any = Refined.make(quotedAssertion, a)
}
