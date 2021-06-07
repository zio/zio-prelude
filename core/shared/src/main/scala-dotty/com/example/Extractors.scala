package com.example

import scala.quoted.*

// From here: https://github.com/deusaquilus/dotty_test/blob/b6b951ed382a617d2ac5daf265bb41e47377a9cf/src/main/scala/io/getquill/parser/TastyMatchers.scala
object Const {
  def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
    import quotes.reflect.*
    def rec(term: Term): Option[T] =
      term match {
        case Literal(c)         => Some(c.value.asInstanceOf[T])
        case Block(Nil, e)      => rec(e)
        case Typed(e, _)        => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _                  => None
      }

    rec(expr.asTerm)
  }
}
