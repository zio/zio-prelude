package com.example

import scala.quoted.{ given, * }

object PrintTreeMacro {
  inline def printTree(inline n: Any): Unit = ${ printTreeImpl('n) }

  def printTreeImpl(n: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect.{ given, * } // TASTY reflection, we need this to be able to go from Expr[T] => Term
    '{
      println("============Tree Structure============")
      println(${ Expr(Printer.TreeStructure.show(n.asTerm.underlyingArgument)) })
      println("============Tree Ansi Code============")
      println(${ Expr(Printer.TreeAnsiCode.show(n.asTerm.underlyingArgument)) })
      println("============Tree Code============")
      println(${ Expr(Printer.TreeCode.show(n.asTerm.underlyingArgument)) })
      println("============Tree Short Code============")
      println(${ Expr(Printer.TreeShortCode.show(n.asTerm.underlyingArgument)) })
      println("============Expr show============")
      println(${ Expr(n.show) })
    }
  }
}
