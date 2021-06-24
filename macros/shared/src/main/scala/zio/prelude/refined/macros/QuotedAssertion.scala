package zio.prelude.refined.macros

import zio.prelude.refined.Assertion

import scala.annotation.StaticAnnotation

final case class QuotedAssertion(assertion: Assertion[_]) extends StaticAnnotation
