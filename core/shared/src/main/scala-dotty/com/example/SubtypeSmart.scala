package com.example

import scala.quoted.*

final case class SubtypeSmart[-A, T](assertion: Assertion[A])
object SubtypeSmart {
  given [A, T](using Type[A], Type[T]): FromExpr[SubtypeSmart[A, T]] with {
    def unapply(subtypeSmart: Expr[SubtypeSmart[A, T]])(using Quotes): Option[SubtypeSmart[A, T]] =
      subtypeSmart match {
        case '{ Refined[A, T]($assertion) }      => Some(SubtypeSmart(assertion.valueOrError))
        case '{ SubtypeSmart[A, T]($assertion) } => Some(SubtypeSmart(assertion.valueOrError))
        case _                                   => None
      }
  }
}

extension [A, T](inline subtypeSmart: SubtypeSmart[A, T]) {
  transparent inline def apply(inline n: A): Any = Refined.make(subtypeSmart, n)
}
