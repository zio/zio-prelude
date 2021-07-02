package zio.prelude.refined.macros

import zio.prelude.refined.Assertion

import scala.quoted.*

infix type Refined[A, B] = QuotedAssertion[A, B]

object Refined extends Liftables {
  inline def apply[A, T](inline assertion: Assertion[A]): QuotedAssertion[A, T] = ${ applyImpl[A, T]('assertion) }

  private def applyImpl[A: Type, T: Type](assertionRaw: Expr[Assertion[A]])(using Quotes): Expr[QuotedAssertion[A, T]] = {
    import quotes.reflect.*
    val assertion = assertionRaw.asTerm.underlyingArgument.asExprOf[Assertion[A]]
    '{ QuotedAssertion($assertion) }
  }

  transparent inline def make[A, T](inline quotedAssertion: QuotedAssertion[A, T], inline a: A): Any = ${
    makeImpl('quotedAssertion, 'a)
  }

  private def makeImpl[A: Type, T: Type](quotedAssertionRaw: Expr[QuotedAssertion[A, T]], a: Expr[A])(
    using Quotes
  ): Expr[Any] = {
    import quotes.reflect.*
    val quotedAssertion = quotedAssertionRaw.asTerm.underlyingArgument.asExprOf[QuotedAssertion[A, T]]
    val assertion    = quotedAssertion.valueOrError.assertion
    a match {
      case LiteralUnlift(x) =>
        assertion(x.asInstanceOf[A]) match {
          case Right(_)    => '{ $a.asInstanceOf[T] }
          case Left(error) => report.throwError(error.render(a.toString))
        }
      case _               =>
        '{
          ${ Expr(assertion) }.apply($a) match {
            case Right(_) => Right($a.asInstanceOf[T])
            case Left(m)  => Left(m.render($a.toString))
          }
        }
    }
  }
}
