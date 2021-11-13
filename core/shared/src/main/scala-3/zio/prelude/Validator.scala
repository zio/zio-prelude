package zio.prelude

import scala.quoted._

abstract class Validator[A](
  assertion: Assertion[A]
) (using FromExpr[A]) {

  final def validate(value: A): Either[AssertionError, A] =
    assertion(value).as(value)

  final def validateInlineImpl(expr: Expr[A])(using Quotes): Expr[A] =
    expr.value match {
      case Some(a) =>
        assertion(a) match {
          case Right(()) => expr
          case Left(err) =>
            quotes.reflect.report.error(err.render(a.toString))
            ???
        }
      case None =>
        quotes.reflect.report.error("Validator args must be statically known")
        ???
    }

}
