package zio.prelude

import scala.quoted._

abstract class Validator[A](
  val assertion: Assertion[A]
) extends Liftables {

  final def validateInlineImpl(expr: Expr[A])(using Quotes, Type[A]): Expr[A] = {
    expr match {
      case LiteralUnlift(a) =>
        assertion(a) match {
          case Right(()) => expr
          case Left(err) =>
            quotes.reflect.report.error(err.render(a.toString))
            ???
        }
      case _ =>
        quotes.reflect.report.error("Validator args must be statically known")
        ???
    }
  }

}
