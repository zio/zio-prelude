package com.example

import scala.quoted.{ given, * }

object PrintTypeMacro {
  inline def printType[T]: Unit = ${ printTypeImpl[T] }

  def printTypeImpl[T: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.{ given, * } // TASTY reflection, we need this to be able to go from Expr[T] => Term
    '{
      println("============TypeRepr Structure============")
      println(${ Expr(Printer.TypeReprStructure.show(TypeRepr.of[T])) })
      println("============TypeRepr Code============")
      println(${ Expr(Printer.TypeReprCode.show(TypeRepr.of[T])) })
      println("============TypeRepr Short Code============")
      println(${ Expr(Printer.TypeReprShortCode.show(TypeRepr.of[T])) })
      println("============TypeRepr Ansi Code============")
      println(${ Expr(Printer.TypeReprAnsiCode.show(TypeRepr.of[T])) })
      println("============TypeRepr show============")
      println(${ Expr(TypeRepr.of[T].show) })
      println("============TypeTree show============")
      println(${ Expr(TypeTree.of[T].show) })
    }
  }
}
