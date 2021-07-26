package zio.prelude.refined

import scala.annotation.StaticAnnotation

final case class QuotedAssertion(assertion: Assertion[_]) extends StaticAnnotation
