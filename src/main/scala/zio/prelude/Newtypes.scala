package zio.prelude

trait Newtypes {
  object MultInt extends Subtype[Int]
  type MultInt = MultInt.Type

  object MultLong extends Subtype[Long]
  type MultLong = MultLong.Type

  object Conj extends Subtype[Boolean]
  type Conj = Conj.Type

  sealed case class First[A](value: A)
  sealed case class Last[A](value: A)

  sealed case class Min[A](value: A)
  sealed case class Max[A](value: A)
}
