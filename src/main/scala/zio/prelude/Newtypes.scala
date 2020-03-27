package zio.prelude

trait Newtypes {
  object Sum extends SubtypeF
  type Sum[A] = Sum.Type[A]

  object Prod extends SubtypeF
  type Prod[A] = Prod.Type[A]

  object Or extends Subtype[Boolean]
  type Or = Or.Type

  object And extends Subtype[Boolean]
  type And = And.Type

  object First extends SubtypeF
  type First[A] = First.Type[A]

  object Last extends SubtypeF
  type Last[A] = Last.Type[A]

  object Min extends SubtypeF
  type Min[A] = Min.Type[A]

  object Max extends SubtypeF
  type Max[A] = Max.Type[A]
}
