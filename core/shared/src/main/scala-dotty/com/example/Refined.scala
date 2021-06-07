package com.example

import scala.quoted.*

infix type Refined[A, B] = SubtypeSmart[A, B]
object Refined {
  inline def apply[A, T](inline assertion: Assertion[A]): SubtypeSmart[A, T] = ${ applyImpl[A, T]('assertion) }

  private def applyImpl[A: Type, T: Type](assertionRaw: Expr[Assertion[A]])(using Quotes): Expr[SubtypeSmart[A, T]] = {
    import quotes.reflect.*
    val assertion = assertionRaw.asTerm.underlyingArgument.asExprOf[Assertion[A]]
    '{ SubtypeSmart($assertion) }
  }

  transparent inline def make[A, T](inline subtypeSmart: SubtypeSmart[A, T], inline a: A): Any = ${
    makeImpl('subtypeSmart, 'a)
  }

  private def makeImpl[A: Type, T: Type](subtypeSmartRaw: Expr[SubtypeSmart[A, T]], a: Expr[A])(using
    Quotes
  ): Expr[Any] = {
    import quotes.reflect.*
    val subtypeSmart = subtypeSmartRaw.asTerm.underlyingArgument.asExprOf[SubtypeSmart[A, T]]
    val assertion    = subtypeSmart.valueOrError.assertion
    a match {
      case Const(x) =>
        assertion(x) match {
          case Right(_)    => '{ $a.asInstanceOf[T] }
          case Left(error) => report.throwError(error)
        }
      case _               =>
        '{
          ${ Expr(assertion) }.apply($a) match {
            case Right(_) => Right($a.asInstanceOf[T])
            case Left(m)  => Left(m)
          }
        }
    }
  }
}
