package zio.prelude

sealed trait AnyType[A]
object AnyType {
  implicit def apply[A]: AnyType[A] = new AnyType[A] {}
}
