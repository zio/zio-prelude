package zio.prelude

import scala.quoted._

abstract class Validator[A](
  val assertion: Assertion[A]
) {

  final def validateInlineImpl(expr: Expr[A])(using Quotes): Expr[A] = {
    given FromExpr[A] = summon[FromExpr[A]]
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

}
