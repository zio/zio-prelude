package zio.prelude.refined.macros

import zio.prelude.refined.Assertion

import scala.annotation.StaticAnnotation

case class QuotedAssertion(assertion: Assertion[_]) extends StaticAnnotation
