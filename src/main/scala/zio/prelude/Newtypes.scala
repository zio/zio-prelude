package zio.prelude

trait Newtypes {
  object MultInt extends Subtype[Int]
  type MultInt = MultInt.Type

  object MultLong extends Subtype[Long]
  type MultLong = MultLong.Type

  object Conj extends Subtype[Boolean]
  type Conj = Conj.Type

  object First extends SubtypeF
  type First[A] = First.Type[A]

  object Last extends SubtypeF
  type Last[A] = Last.Type[A]

  object Min extends SubtypeF
  type Min[A] = Min.Type[A]

  object Max extends SubtypeF
  type Max[A] = Max.Type[A]
}
